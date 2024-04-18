        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR    QQQ   IIIII  N   N   QQQ   RRRR   Y   Y   *~
            *  A   A  R   R  Q   Q    I    NN  N  Q   Q  R   R   Y Y    *~
            *  AAAAA  RRRR   Q   Q    I    N N N  Q   Q  RRRR     Y     *~
            *  A   A  R   R  Q Q Q    I    N  NN  Q Q Q  R  R     Y     *~
            *  A   A  R   R   QQQ   IIIII  N   N   QQQ   R   R    Y     *~
            *                    Q                    Q                 *~
            *-----------------------------------------------------------*~
            * ARQINQRY - A/R Inquiry Driver Program.                    *~
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
            * 12/24/86 ! Original                                 ! ERN *~
            * 06/10/88 ! Added aging by source document date      ! ERN *~
            * 12/21/88 ! Added CUSTOMER free text (Control Cable).! JIM *~
            * 08/16/89 ! Added Multi-Currency flag and Statutory  ! MLJ *~
            *          !   currency code to sub argument lists.   !     *~
            * 11/30/89 ! Last purged dates for invoices & checks, ! JDH *~
            *          !  1st look to SYSFILE2 written by ARMPURGE!     *~
            *          !  then to SYSFILE2 written by SYSPURGE.   !     *~
            * 06/13/90 ! Repaired customer text problem, added    ! JDH *~
            *          !  find cust by invoice, removed PF4.      !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 11/13/92 ! Added call to Customer Credit (ARQCUSCR) ! JIM *~
            * 11/13/92 ! Access Cust even if no transactions.     ! JIM *~
            * 03/15/93 ! PRR 12641 Update CUSTOMER text pointer   ! JIM *~
            *          !   immediately following text input/edit. ! JIM *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acctie$, acct$(2)12,         /* G/L Ranges                 */~
            ageper$1,                    /* Aging Per Disc or Net Date */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            asof$8, asofu$8,             /* As Of Date                 */~
            billto$9, billtoname$30,     /* Bill-to Code and Name      */~
            billtoxref$9,                /* Bill-to X-Ref              */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            curr$1,                      /* Multi-Currency usage flag  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(10),               /* Screen Map for PLOWCODE    */~
            dtldoc$8, dtlsrce$1,         /* Pass thrus for Dtl Dispaly */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            info$79,                     /* Misc Informative text      */~
            inpmessage$79,               /* Informational Message      */~
            ix(1),                       /* Include/Exclude for PLOW   */~
            ix$(1)1,                     /* Include/Exclude for PLOW   */~
            last_chk$8, last_inv$8,      /* Last Purged Dates          */~
            last_stl$8,                  /* Last Settling Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            ofac$(3)1, options$(3)79,    /* Inquiry Options            */~
            pf16$16, pf14$30, pf8$26,    /* PF Key Literals            */~
            pf25$25, pf25key$1,          /* PF Key Literals            */~
            pf26$13, pf26key$1,          /* PF 26 Literals             */~
            plowhdr$(3)79,               /* Plowcode Headers           */~
            plowkey$50,                  /* A Plow Key                 */~
            readkey$50,                  /* Read Key                   */~
            savebillto$9,                /* A place to hold            */~
            stat_curr_code$4,            /* STATUTORY Currency Code    */~
            shipto$9,                    /* Ship-to Customer Number    */~
            text$(392,1)70, textid$4     /* Text Routine Elements      */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* File Read Status Flag      */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #5  ! ARIMASTR ! Invoice Master File                      *~
            * #6  ! ARILINES !                                          *~
            * #7  ! CRCMASTR ! Check Master File                        *~
            * #8  ! CRCLINES !                                          *~
            * #9  ! TXTFILE  ! System Text File                         *~
            * #10 ! GLMAIN   ! General Ledger Master File               *~
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

            select  #5, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup,    ~
                            key  4, keypos = 1783, keylen =  26

            select  #6, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select # 7, "CRCMASTR",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 17

            select  #8, "CRCLINES",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 21

            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #10,  "GLMAIN",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1,  fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2,  fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3,  fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 5,  fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 7,  fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 9,  fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10,  fs%(10), f2%(10), 0%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            select printer

*        Check for Multi-Currency usage and get STATUTORY currency code
            curr$ = "N"
            stat_curr_code$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
            if f1%(2) = 0% then L09120        /* Multi-Currency not used */
                get #2 using L09108, curr$, stat_curr_code$
L09108:             FMT POS(21), CH(1), CH(4)

L09120:     readkey$  = "ARM.LAST.SETTLING"
            last_stl$ = blankdate$
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09160, last_stl$
L09160:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)

            last_chk$, last_inv$ = " "
            readkey$  = "ARM.PURGE." & hex(00)
L09174:     call "PLOWNXT1" (#2, readkey$, 10%, f1%(2))
                if f1%(2) = 0% then L09188
            get #2 using L09180, tempi$, tempc$
L09180:         FMT POS(31), CH(6), POS(51), CH(6)
            if (last_inv$ = " "  or last_inv$ = blankdate$) and ~
                tempi$ <>  " " then last_inv$ = tempi$
            if (last_chk$ = " "  or last_chk$ = blankdate$) and ~
                tempc$ <>  " " then last_chk$ = tempc$
            if last_inv$ = " "   or last_inv$  = blankdate$ or ~
               last_chk$ = " "   or last_chk$  = blankdate$ then L09174
L09188:     if last_inv$ <> " " and last_inv$ <> blankdate$ and ~
               last_chk$ <> " " and last_chk$ <> blankdate$ then L09240

            tempdate$ = " "
            readkey$  = "LAST PURGE DATES"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09160, tempdate$
            if tempdate$ = " " then tempdate$ = blankdate$
            if last_inv$ = " " or last_inv$ = blankdate$ then ~
               last_inv$ = tempdate$
            if last_chk$ = " " or last_chk$ = blankdate$ then ~
               last_chk$ = tempdate$
L09240:     call "DATEFMT" (last_inv$)
            call "DATEFMT" (last_chk$)

*        Set up Options Display Strings
            options$(1) = "Inquiry Options:  (PF- 9) Trial Balance"
            options$(2) = "                  (PF-10) Invoices     "
            options$(3) = "                  (PF-11) Checks       "
            str(options$(1),45) = "Last Settled through " & last_stl$
            str(options$(2),45) = "Last Purged through  " & last_inv$
            str(options$(3),45) = "Last Purged through  " & last_chk$

            mat ix = zer : init (" ") ix$()
            descr_map( 1) =  10.08  : descr_map( 2) = 01
            descr_map( 3) =  01.09  : descr_map( 4) = 10
            descr_map( 5) =  34.16  : descr_map( 6) = 20
            descr_map( 7) =  18.16  : descr_map( 8) = 37
            descr_map( 9) = 521.061 : descr_map(10) = 54

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf8$=" ": pf16$="(16)Exit Program"
            pf14$="(14)Find Customer by Invoice #"
            pf26$ = " "
            init(" ") errormsg$, inpmessage$, asof$, aging$(), ageper$,  ~
                      billto$, billtoname$, acctie$, acct$()
            mat aging% = zer
                aging%(1) =  30% : aging%( 2) =  60% : aging%(3) =  90%
                aging%(4) = 120% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%
            textid$ = all(hex(ff))
            call "TXTFUTIL" (#9, f2%(9), "INTL", textid$)

            for fieldnr% = 1% to 4%
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10310
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                errormsg$ = " "
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10280
L10230:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10230
L10280:               if keyhit%  =  5% then gosub next_billto
                      if keyhit%  = 14% then gosub find_cust
                      if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10190
L10310:         gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%

            call "ARMDATES" (asofu$, "A/R TRIAL BALANCE INQUIRY",        ~
                             aging%(), 1%, 0%, aging$(), 99%)


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf8$ = "(8)Modify Aging Parameters"
            pf16$ = "(16)Exit Program"
            pf25$ = "(25)Manage CUSTOMER text"
            pf26$ = "(26)Cust. Cr."
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
            errormsg$ = " "  :  ret% = 0%
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then gosub next_billto
                  if keyhit%  =  8 then gosub aging_parameters
                  if keyhit%  =  9 then gosub tb_inquiry
                  if keyhit%  = 10 then gosub invoice_inquiry
                  if keyhit%  = 11 then gosub check_inquiry
                  if keyhit%  = 14 then gosub find_cust
                  if keyhit%  = 16 then       exit_program
                  if keyhit%  = 25 then gosub cust_text
                  if keyhit%  = 26% then gosub customer_credit_screen
                  if keyhit% <>  0 then       edtpg1
L11210:     fieldnr% = min(max(1%, cursor%(1) - 5%), 5%)
            if fieldnr% = lastfieldnr% then edtpg1
            if fieldnr% <> 5% then L11250
                  gosub aging_parameters  :  goto L11340
L11250:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  pf25$, pf8$, pf16$, pf25$ = " "
L11280:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =   1% then gosub startover
                  if keyhit%  =   5% then gosub next_billto
                  if keyhit%  =  14% then gosub find_cust
                  if keyhit%  <>  0% then L11280
            gosub'151(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11280
L11340:              lastfieldnr% = fieldnr%
                     goto L11210


        aging_parameters
            call "ARMDATES" (asofu$, "A/R TRIAL BALANCE INQUIRY",        ~
                             aging%(), 1%, 0%, aging$(), u3%)
            if u3% <> 1% then return
                return clear all
                goto   inputmode


        next_billto       /* Find and Load Next Bill-to on File        */
            savebillto$ = billto$
L11480:     call "PLOWNEXT" (#3, billto$, 0%, f1%(3))
            if f1%(3) = 1% then L11540
                errormsg$ = "End of Customer File Reached."
                billto$   = savebillto$
                call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
                return
L11540:     get #3 using L11550, billtoxref$, textid$
L11550:         FMT POS(780), CH(9), CH(4)
            if billtoxref$ <> billto$ then L11480
                call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$)
                errormsg$ = " "  :  gosub test_billto
                if errormsg$ = " " then return else L11480


        tb_inquiry
            if ar% = 0% then return
L11630:     call "ARQTBSUB" (billto$, aging%(), asofu$, ageper$, #1, #3, ~
                             "Y", ret%, dtldoc$, shipto$, dtlsrce$,      ~
                             curr$, stat_curr_code$, acctie$, acct$(1),  ~
                             acct$(2))
            if ret% = 0% then return
                if dtlsrce$ = "I" then gosub invoice_inquiry else        ~
                                       gosub check_inquiry
                ret% = 1%
                goto L11630

        invoice_inquiry
            if invs% = 0% then L11730
               call "ARQINVSB" (billto$, asofu$, #3, #5, #6, ret%,       ~
                                dtldoc$, shipto$, curr$, stat_curr_code$)
               return
L11730:     call "ASKUSER" (2%, "INVOICE DISPLAY",                       ~
                      "The Invoice requested is not on file.",           ~
                      " ", "Press any PF Key to Continue...")
            return

        check_inquiry
            if chks% = 0% then L11800
                call "ARQCHKSB" (billto$, asofu$, #3, #7, #8, ret%,      ~
                            dtldoc$, curr$, stat_curr_code$)
                return
L11800:     call "ASKUSER" (2%, "CHECK DISPLAY",                         ~
                      "The Check requested is not on file.",             ~
                      " ", "Press any PF Key to Continue...")
            return

        cust_text
            call "TXTINSUB" (#9, f2%(9), "012", billto$ &                ~
                 " (" & billtoname$ & ")", textid$, text$())

*        SAVE_CUSTOMER_TEXT
            call "READ101" (#3, billto$, f1%(3%))
            if f1%(3%) = 0% then return /* Should never happen */
            put #3 using L11900, textid$
L11900:         FMT POS(789), CH(4)
            rewrite #3
            call "TXTFUTIL" (#9, f2%(9%), "TOS2", textid$)
            call "TXTFUTIL" (#9, f2%(9%), "LOAD", textid$)
            return

        find_cust
           info$ = hex(06) & "FIND CUSTOMER - Please Select Invoice"
           init (hex(00)) plowkey$
           plowhdr$(1) = "  Invoice  Customer  Sales Order      Purchase ~
        ~Order   Inv Date"
           call "PLOWCODE" (#5, plowkey$, info$, 9000%, 1.32, f1%(5),    ~
                            plowhdr$(), 0, 1, ix(), ix$(), "d", " ", #5, ~
                            descr_map())
           if f1%(5) = 0% then return
               call "READ100" (#3, str(info$, 10, 9), f1%(3))
               if f1%(3) <> 0% then get #3, using L12086, billto$
L12086:             FMT POS(780), CH(9)
               gosub'151(1%, 2%)
               return

        customer_credit_screen
            call "ARQCUSCR" (billto$)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1% : if fieldnr% > 1% then enabled% = 0%
                  on fieldnr% gosub L20130,         /* Bill-to Number   */~
                                    L20180,         /* As Of Date       */~
                                    L20240,         /* Age Per Date     */~
                                    L20300          /* GL Range         */
                     return

L20130
*        Bill-to Customer                      BILLTO$
            inpmessage$ = "Enter Bill-to Customer for Inquiry; Leave" &  ~
                          " blank to search Customer file."
            return

L20180
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            return

L20240
*        Age Per Date                          AGEPER$
            inpmessage$ = "'D' to Age per Cash Discount Date, 'N' by" &  ~
                          " Net Due Date, or 'I' by Invoice Date."
            if ageper$  = " " then ageper$ = "N"
            return

L20300
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
                if pf25$ = " " then pf25key$ = hex(ff)                   ~
                               else pf25key$ = hex(19)
                if pf26$ = " " then pf26key$ = hex(ff)                   ~
                               else pf26key$ = hex(1a)
                  str(line2$,62%) = "ARQINQRY: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  if edit% = 1% then lfac$(5) = hex(9c)
                  init(hex(9c)) ofac$()
                  if edit% = 1% or fieldnr% > 0% then L40170
                          init(hex(8c)) ofac$()
                          if ar%   = 1% then ofac$(1) = hex(84)
                          if invs% = 1% then ofac$(2) = hex(84)
                          if chks% = 1% then ofac$(3) = hex(84)
L40170:           on fieldnr% gosub L40250,         /* Bill-to Customer */~
                                    L40250,         /* As Of Date       */~
                                    L40250,         /* Aging per date   */~
                                    L40250          /* GL Ranges        */
                  goto L40320

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40250:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40320:     accept                                                       ~
               at (01,02), "A/R Trial Balance Inquiry",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bill-to Customer",                           ~
               at (06,30), fac(lfac$( 1)), billto$              , ch(09),~
               at (06,49), fac(hex(8c))  , billtoname$          , ch(30),~
                                                                         ~
               at (07,02), "As Of Date",                                 ~
               at (07,30), fac(lfac$( 2)), asof$                , ch(08),~
               at (07,49), "Last Settled on ",                           ~
               at (07,65), fac(hex(8c)), last_stl$              , ch(08),~
                                                                         ~
               at (08,02), "Date to Base Agings On",                     ~
               at (08,30), fac(lfac$( 3)), ageper$              , ch(01),~
               at (08,49), "'D'isc  'N'et  'I'nvoice",                   ~
                                                                         ~
               at (09,02), "Include/Exclude G/L Accts:",                 ~
               at (09,30), fac(lfac$( 4)), acct$(1)             , ch(12),~
               at (09,43), "to",                                         ~
               at (09,46), fac(lfac$( 4)), acct$(2)             , ch(12),~
               at (09,60), "Include/Exclude",                            ~
               at (09,76), fac(lfac$( 4)), acctie$              , ch(01),~
                                                                         ~
               at (10,02), "Aging Parameters:",                          ~
               at (10,20), "1)", at(11,20), "2)", at(12,20), "3)",       ~
               at (13,20), "4)", at(14,20), "5)", at(10,30), "Days",     ~
               at (10,23), fac(lfac$( 5)), aging%( 1)       ,pic(-#####),~
               at (11,23), fac(lfac$( 5)), aging%( 2)       ,pic(-#####),~
               at (12,23), fac(lfac$( 5)), aging%( 3)       ,pic(-#####),~
               at (13,23), fac(lfac$( 5)), aging%( 4)       ,pic(-#####),~
               at (14,23), fac(lfac$( 5)), aging%( 5)       ,pic(-#####),~
               at (10,37), fac(lfac$( 5)), aging$( 1)           , ch(20),~
               at (11,37), fac(lfac$( 5)), aging$( 2)           , ch(20),~
               at (12,37), fac(lfac$( 5)), aging$( 3)           , ch(20),~
               at (13,37), fac(lfac$( 5)), aging$( 4)           , ch(20),~
               at (14,37), fac(lfac$( 5)), aging$( 5)           , ch(20),~
                                                                         ~
               at (16,02), fac(ofac$(1)) , options$(1)          , ch(79),~
               at (17,02), fac(ofac$(2)) , options$(2)          , ch(79),~
               at (18,02), fac(ofac$(3)) , options$(3)          , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,51), fac(hex(8c)), pf26$,                          ~
               at (23,02), "(5)Next Bill-to",                            ~
               at (22,65), "(13)Instructions",                           ~
               at (23,19), fac(hex(8c)), pf14$,                          ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (24,19), fac(hex(8c)), pf25$,                          ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(0001ff0508090a0b0d0e0f10) & pf25key$ &     ~
                     pf26key$),                                          ~
                     key(keyhit%)

               if keyhit% <> 13 then L40850
                  call "MANUAL" ("ARQINQRY")
                  goto L40320

L40850:        if keyhit% <> 15 then L40890
                  call "PRNTSCRN"
                  goto L40320

L40890:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50136,         /* Bill-to Customer */~
                                    L50410,         /* As Of Date       */~
                                    L50550,         /* Age Per          */~
                                    L50600          /* G/L Ranges       */
                  return

L50136: test_billto
            billtoname$ = hex(06) & "Select Bill-to Customer"
            call "GETCODE" (#3, billto$, billtoname$, 0%, 1.30, f1%(3))
            if f1%(3) = 1% then L50180
                errormsg$ = "Bill-to Number not on file." : return
L50180:     get #3 using L50190, billxref$, textid$
L50190:         FMT POS(780), CH(9), CH(4)
            call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$)
            plowkey$ = str(billto$) & hex(00)
            call "PLOWNEXT" (#1, plowkey$, 9%, ar%  )
            plowkey$ = str(billto$) & hex(00)
            call "PLOWNEXT" (#5, plowkey$, 9%, invs%)
            if invs% = 1% or billxref$ <> billto$ then L50350
                call "REDALT0" (#3, billto$, 4%, f1%(3)) /* Primer */
L50260:         if f1%(3) = 0% then L50350
                get #3 using L50280, shipto$, billxref$
L50280:              FMT CH(9), POS(780), CH(9)
                if billxref$ <> billto$ then L50350
                     plowkey$ = str(shipto$) & hex(00)
                     call "PLOWNEXT" (#5, plowkey$, 9%, invs%)
                     if invs% = 1% then L50350
                call "READNEXT" (#3, f1%(3))
                goto L50260
L50350:     plowkey$ = str(billto$) & hex(00)
            call "PLOWNEXT" (#7, plowkey$, 9%, chks%)
            return

L50410
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)      :  asofu$ = asof$
                call "DATUNFMT" (last_stl$)
                if asof$ <= last_stl$ then errormsg$ =                   ~
                     "As Of can not be on or before last settling date."
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                if edit% = 2% then                                       ~
                     call "ARMDATES" (asofu$, " ", aging%(), 1%, 1%,     ~
                                                           aging$(), 99%)
                return

L50550
*        Age Per                               AGEPER$
            if pos("DIN" = ageper$) > 0% then return
                errormsg$ = "Enter 'D', 'N', or 'I'."
                return

L50600
*        Select G/L Account
            if acct$(1) = "ALL" then L50760
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#10, acct$(1), temp$, 0%, 0, f1%(10))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#10, acct$(2), temp$, 0%, 0, f1%(10))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                return
L50760:     acct$(2) = " "
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
            call "TXTFUTIL" (#9, f2%(9%), "INTL", textid$)
            call "SETPRNT" ("ARM004", " ", 0%, 1%)
            end
