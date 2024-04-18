        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  R   R  C   C    I    NN  N  P   P  U   U    T     *~
            *  C      RRRR   C        I    N N N  PPPP   U   U    T     *~
            *  C   C  R   R  C   C    I    N  NN  P      U   U    T     *~
            *   CCC   R   R   CCC   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCINPUT - Allows entry of Customer Checks or Adjustment  *~
            *            Vouchers and line item distribution to A/R,    *~
            *            Direct Sales, or G/L.                          *~
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
            * 12/04/86 ! Original                                 ! ERN *~
            * 10/29/87 ! Corrected a bug in display of modified   ! DAW *~
            *          ! due date                                 !     *~
            * 12/04/87 ! Added Multi-currency, CURMASTR, CRCCURCY,! JIM *~
            *          !   CURCONVR.                              !     *~
            * 08/25/88 ! Added Rounding to currency calculations. ! RJM *~
            * 10/03/88 ! Fixed Rounding to currency calculations. ! JDH *~
            * 12/13/88 ! Fixed Problem with Multi-C turned off.   ! JDH *~
            * 07/12/89 ! Adjed Rounding to currency calculations. ! JDH *~
            * 08/28/89 ! Added "N", CURR$, & STATUTORY$ to calling! MLJ *~
            *          !   args of ARQTBSUB (due to ARQINQRY chgs)!     *~
            * 10/19/89 ! Increased PIC in places, changed 2nd date! JDH *~
            *          !  on Adj Due Date line of line item screen!     *~
            *          !  for type 'A' & 'P' to orig due date.    !     *~
            * 02/08/90 ! I can't believe I'm fixing rounding again! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            *          !  & improved Posting Type input msg.      !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            *          !  Added Buffer sub (ARQTBPRM) between     !     *~
            *          !  here and ARQTBSUB to allow user to view !     *~
            *          !  and or modify TB parameters.  Can also  !     *~
            *          !  select whether view is one time, at     !     *~
            *          !  customer change or on every request for !     *~
            *          !  trial balance info. (hopefully this     !     *~
            *          !  wont offend anyone too much).           !     *~
            *          ! Added same G/L stuff to AUTO APPLY fnct. !     *~
            *          !  and added filter screen for check-off   !     *~
            *          !  load.  CAUTION.  Has its own screen     !     *~
            *          !  (107) but uses def/enable (054) and     !     *~
            *          !  validation (154) from AUTO PARMS. Just  !     *~
            *          !  a subset, after all, and they share     !     *~
            *          !  AUTO LOAD anyway.                       !     *~
            *          ! Minor Bug in date ranging.  Invoking     !     *~
            *          !  TESTRNGE and then testing exculsively   !     *~
            *          !  could have caused one day less than     !     *~
            *          !  expected to be included.                !     *~
            * 09/24/92 ! Shortened input message (over 79 chars). ! JDH *~
            * 11/13/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 01/06/93 ! Increased Summary, Bill-To variables (12)! JIM *~
            * 02/22/01 ! Mod to add SHOSTAT on screen if settlement!CMG *~
            *          !    part of Nor Bankruptcy.       (EWD001)!     *~            
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            adjduef$8,                   /* Adjusted Due Date- Fmtd    */~
            auto_asof$8, auto_asofu$,    /* Auto- As Of Date           */~
            auto_amts(3,3),              /*       Amts                 */~
            auto_amts$(3)10,             /*       Amts to Apply        */~
            auto_currchk$1,              /*       Check Currency Only  */~
            auto_hdrs$(4)12,             /*       Column Headings      */~
            auto_left$(2)10,             /*       Amts Remaining       */~
            auto_ranges$(5)8,            /*       Range Info           */~
            auto_stlmnt$(4)8,            /*       Range Info           */~
            auto_acct$(4)12,             /*       Range Info           */~
            auto_acctie$1,               /*       Range Info           */~
            bals(3), cbals(3), sbals(3), /* Balances                   */~
            banknr$10,                   /* Bank Number                */~
            billto$9, billto_name$30,    /* Bill-to Number & Name      */~
            billto_ar$12,                /* Open A/R for Bill-to       */~
            billto_bal$13,               /* Bill-to Balance w/ postngs */~
            billto_msg$30,               /* 'Direct Pstngs Exist' Msg  */~
            billto_other$12,             /* Other Postings to Bill-to  */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            chkdate$8,                   /* Check Date                 */~
            chknr$8,                     /* Check Number               */~
            convdate$6,                  /* Currency conversion date   */~
            cracctdescr$32,              /* Credit Account Descr       */~
            credits_first$1,             /* Auto- Process Crs First?   */~
            curbal$10,                   /* Current Balance            */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$32,     /* Currency code& description */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dflt_ar_acct$12,             /* A/R Account Default        */~
            dflt_sales_acct$12,          /* Sales Account Default      */~
            disca$10, discu$10,          /* Line Item Disc Amts        */~
            discpct$6,                   /* Line Item Cash Disc %      */~
            duedatef$10,                 /* Due Date- Formatted        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            force_balance$1,             /* Auto- Force Balance?       */~
            gross$10,                    /* Gross Payment Amount       */~
            hdr$(8)12,                   /* Summary Display Headings   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(30)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            net$10,                      /* Net Payment Amount         */~
            netchk$10,                   /* Net Check Amount           */~
            newbal$11,                   /* New Balance                */~
            pf$(3)79, pfkey$32,          /* PF Key Descriptors         */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            posted$6,                    /* Transaction Post Date      */~
            postedmsg$20,                /* Line Posted Message        */~
            postmark$8, postmarku$6,     /* Post Mark Date/ Unformtd   */~
            readkey$50,                  /* Miscellaneous Read/Plow Key*/~
            seq$4,                       /* Line Item Sequence         */~
            session$6,                   /* Session ID                 */~
            sum_disca$12, sum_discu$12,  /* Posting Summary- Discounts */~
            sum_net$12, sum_ar$12, sum_ars$12,   /*          Chk Amts  */~
            sum_gl$12,                   /*                  Sale, G/L */~
            sfac$(30)1, summary$(30)79,  /* Summary Display and FACs   */~
            statutory$4,                 /* Statutory currency code    */~
            temp%(1),                    /* Search Target              */~
            tcurrmsg$21,                 /* Transaction Currency Msg   */~
            typedescr$20,                /* Line Type Description      */~
            userid$3                     /* Current User Id            */~

        dim                              /* LINE ITEM VARIABLES        */~
            adjdue$(1500)6,              /* Adjusted Due Dates         */~
            auto_idx$(1500)12,           /* Auto Index / Sort          */~
            cracct$(1500)12,             /* Credit Accounts            */~
            currency$(1500)4,            /* Line Currency              */~
            conveqv(1500),               /* Conversion Factor, then    */~
            convunt(1500),               /* Conversion Factor, then    */~
            convdate$(1500)6,            /* Conv. Effective Date, then */~
            conveqv1(1500),              /* Conversion Factor, Now     */~
            convunt1(1500),              /* Conversion Factor, Now     */~
            convdate1$(1500)6,           /* Conv. Effective Date, Now  */~
            disca(1500), discu(1500),    /* Discs Taken- Allowed & Not */~
            discpct(1500),               /* Cash Discount Percent      */~
            duedate$(1500)6,             /* Discount Due Dates         */~
            net(1500),                   /* Net Payment Amounts        */~
            other(1500),                 /* Other Postings Totals      */~
            po$(1500)16,                 /* Purchase Order Numbers     */~
            prev(1500),                  /* Previous Balance           */~
            srcedoc$(1500)8,             /* Source Doc Applied Against */~
            srcetype$(1500)2,            /* Srce Doc Srce and Type     */~
            stlmnt$(1500)12,             /* Stlmnt #s applied to       */~
            type$(1500)1                 /* Posting Type Indicator     */


/* (EWD001) */
        dim readgen$24                   /* Gencode Readkey            */

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! UPDSESSN ! Session Control File                     *~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            * #04 ! GLMAIN   ! General Ledger Chart Of Accounts File.   *~
            * #05 ! CRCMASTR ! Cash Receipts Check Header File          *~
            * #06 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #09 ! CRCBUFFR ! Cash Receipts Buffer- Checks             *~
            * #10 ! CRCBUF2  ! Cash Receipts Buffer- Distribution       *~
            * #11 ! GENCODES ! Master System Table File                 *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! CRCMSCUR ! Multi-Currency Master Information        *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #51 ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "UPDSESSN",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #02,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #04, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #05, "CRCMASTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  17

            select #06, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select #09, "CRCBUFFR",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 17,                      ~
                        alt key  1, keypos =  201, keylen =  23

            select #10, "CRCBUF2",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 21,                      ~
                        alt key  1, keypos =  201, keylen =  33

            select #11, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                        
            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11


            select #42, "CRCMSCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

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

            select #51, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

        call "SHOSTAT" ("Cash Receipts Entry: Opening Files")
            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ),   0%, rslt$(1 ))
            call "OPENCHCK" (#02, fs%(2 ), f2%(2 ),   0%, rslt$(2 ))
            call "OPENCHCK" (#03, fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#04, fs%(4 ), f2%(4 ),   0%, rslt$(4 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ),   0%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ),   0%, rslt$(6 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ), 100%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 200%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))            
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
            if f1%(2) <> 0% then get #02 using L05150, curr$, statutory$,  ~
                currtype$
L05150:         FMT POS(21), CH(1), CH(4), POS(33), CH(1)
            if curr$ = "Y" then goto L05170
                statutory$ = " " : goto L09000
L05170:         call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
                call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))
                call "OPENCHCK" (#42, fs%(42), f2%(42), 100%, rslt$(42))
                call "OPENCHCK" (#43, fs%(43), f2%(43), 200%, rslt$(43))
                call "OPENCHCK" (#44, fs%(44), f2%(44),   0%, rslt$(44))
                call "OPENCHCK" (#45, fs%(45), f2%(45), 200%, rslt$(45))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ml% = 1500%   /* Maximum Lines allowed on a Check.         */

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Kill any tasking record outstanding for this user.
            plowkey$ = hex(00) & str(userid$)
            call "PLOWAL1" (#09, plowkey$, 1%, 4%, f1%(9))
            if f1%(9) = 0% then L09250
                plowkey$ = str(key(#09,1%),7,9)
                delete #09
                call "ASKUSER" (2%, "RESTARTING",                        ~
                     "Work on Bill-to Customer " & plowkey$ & " was not",~
                     "completed.  Make sure you complete the check that",~
                     "was in process.  Press RETURN to continue....")

L09250
*        Get which Session to place checks into.
            u3% = 1%
            call "UPDUSRLG" ("CRCUPDTE", "CRCINPUT",                     ~
                             "Cash Receipts Entry", "1", session$, u3%,  ~
                             postdate$, " ")
            if u3% <> 0% then exit_program
                str(line2$,46) = "Session: "   & str(session$)   &       ~
                                 " CRCINPUT: " & str(cms2v$,,8)

*        Get Account Numbers from Session Record
            readkey$ = "CRCUPDTE" & str(session$)
            call "READ100" (#01, readkey$, f1%(1))
            get #01 using L09380, dflt_ar_acct$, dflt_sales_acct$
L09380:         FMT POS(166), 2*CH(9)
            call "GLFMT" (dflt_ar_acct$   )
            call "GLFMT" (dflt_sales_acct$)

*        Set up Display Variables and Descriptor Tables
            hdr$(1) = "Line        "
            hdr$(2) = "Type        "
            hdr$(3) = "Settlement# "
            hdr$(4) = "Srce Docmnt "
            hdr$(5) = "Credit Acct "
            hdr$(6) = "  Prev Bal  "
            hdr$(7) = "   Payment  "
            hdr$(8) = "   New Bal  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Check Header Entry.                                       *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, billto$, chknr$, chkdate$, ~
                      banknr$, postmark$, netchk$, str(line2$,,45),      ~
                      billto_name$, billto_ar$, billto_other$,           ~
                      billto_bal$, sum_ar$, sum_disca$, sum_discu$,      ~
                      sum_net$, postmarku$, type$(), srcedoc$(), po$(),  ~
                      srcetype$(), adjdue$(), duedate$(), cracct$(),     ~
                      sum_gl$, billto_msg$, currency$, currdesc$,        ~
                      convdate$, currency$(), convdate$(), convdate1$(), ~
                      sum_ars$
            conveqv, convunt = 1
            mat discpct = zer  :  mat disca = zer  :  mat prev  = zer
            mat net     = zer  :  mat discu = zer  :  mat other = zer
            mat conveqv = con  :  mat conveqv1 = con
            mat convunt = con  :  mat convunt1 = con
            maxlines%, tasking% = 0%


            for fieldnr% = 1% to 7%
L10200:         gosub'051(fieldnr%)
                     if enabled% = 0 then L10320
L10220:         gosub'101(fieldnr%, 1%)
                     if keyhit%  =  1 then gosub startover
                     if keyhit% <>  4 then       L10300
L10250:                   fieldnr% = max(3%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then L10220
                          if fieldnr% = 3% then L10200
                          goto L10250
L10300:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10220
L10320:         gosub'151(fieldnr%)
                     if errormsg$ <> " " then L10220
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Check Header Edit and Posting's Summary.                  *~
            *************************************************************

        check_header
            gosub summarize_postings
            str(line2$,,45) = " "

        check_header2
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  8% then gosub auto_applications
                  if keyhit%  =  9% then gosub check_off_routine
                  if keyhit%  = 12% then gosub delete_check
                  if keyhit%  = 14% then gosub tb_inquiry
                  if keyhit%  = 16% then gosub save_check
                  if keyhit% <>  0% then       check_header2
L11210:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 3% or fieldnr% > 7% then check_header2

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then check_header2
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  if fieldnr%  <> cursor%(1) - 5% then L11210
                  goto check_header2


        summarize_postings
*        Summarize Postings made so far for display on screen.
            sum_net, sum_disca, sum_discu, sum_gl = 0
            stat_sum_net, stat_sum_disca, stat_sum_discu, stat_sum_gl = 0
            if maxlines% = 0% then L11480
                for x% = 1% to maxlines%
                     sum_net = sum_net + net(x%)
                     stat_sum_net   = stat_sum_net +                     ~
                                      round(net(x%) * conveqv, 4)
                     if pos("GS" = type$(x%)) >  0% then                 ~
                                          sum_gl      = sum_gl + net(x%)
                     if pos("GS" = type$(x%)) >  0% then                 ~
                                          stat_sum_gl = stat_sum_gl +    ~
                                            round(net(x%) * conveqv, 4)
                     sum_disca      = sum_disca + disca(x%)
                     stat_sum_disca = stat_sum_disca +                   ~
                                      round(disca(x%) * conveqv, 4)
                     sum_discu      = sum_discu + discu(x%)
                     stat_sum_discu = stat_sum_discu +                   ~
                                      round(discu(x%) * conveqv, 4)
                next x%
          format_summary
L11480:     sum_ar     = sum_net   - (sum_disca + sum_discu) - sum_gl
            sum_ars    = stat_sum_net   - (stat_sum_disca +              ~
                                           stat_sum_discu) - stat_sum_gl
            billto_bal = billto_ar - (sum_ars + billto_other)
            call "CONVERT" (sum_ar    , 2.2, sum_ar$    )
            call "CONVERT" (sum_ars   , 2.2, sum_ars$   )
            call "CONVERT" (sum_disca , 2.2, sum_disca$ )
            call "CONVERT" (sum_discu , 2.2, sum_discu$ )
            call "CONVERT" (sum_gl    , 2.2, sum_gl$    )
            call "CONVERT" (sum_net   , 2.2, sum_net$   )
            call "CONVERT" (billto_bal, 2.2, billto_bal$)
            return


        delete_check
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE CHECK",                         ~
                            "Press RETURN to cancel delete", "-OR-",     ~
                            "Press PF-16 to DELETE check.")
            if u3% <> 16% then return
            readkey$ = str(billto$) & chknr$
            call "DELETE" (#09, readkey$, 17%)
            readkey$ = str(billto$) & str(chknr$)
            call "DELETE" (#10, readkey$, 17%)
            call "DELETE" (#42, readkey$, 17%)
            call "DELETE" (#43, readkey$, 17%)
            call "DELETE" (#45, readkey$, 17%)
            return clear all
            goto remove_tasking


        REM *************************************************************~
            *           L I N E   I T E M   S U M M A R Y               *~
            *-----------------------------------------------------------*~
            * Handles Review, Insert and Edit Mode for Line Items.      *~
            *************************************************************

        line_summary              /* Summary Screen */
            l% = min(max(0, l%), max(0, maxlines% - 14%))
            if maxlines% > 0% then L12110
                c% = 1%
                goto insert_lines
L12110:     inpmessage$ = "To Modify a Line Item, Position Cursor and" & ~
                          " press RETURN."
            errormsg$   = " "

L12150:   gosub'102                        /* Summary Screen Display */
            errormsg$  = " "
            if keyhit% =  1% then gosub startover
            if keyhit% < 2% or keyhit% > 7% then L12260
                if keyhit% = 2% then l% = 0%
                if keyhit% = 3% then l% = maxlines% - 14%
                if keyhit% = 4% then l% = l% - 12%
                if keyhit% = 5% then l% = min(l%+14%, maxlines%-14%)
                if keyhit% = 6% then l% = l% - 1%
                if keyhit% = 7% then l% = min(l%+1%, maxlines%-14%)
                                     l% = max(0%, l%) : goto L12150
L12260:     if keyhit% =  8% then gosub auto_applications
            if keyhit% =  9% then gosub check_off_routine
            if keyhit% = 14% then gosub tb_inquiry
            if keyhit% = 16% then       check_header
            if keyhit% =  0% or                                          ~
               keyhit% = 11% or                                          ~
               keyhit% = 12% then L12330 else L12150
L12330:              c% = min(max(l%-1%, cursor%(1)-4%+l%), maxlines%)
                     if c% = l% - 1% and keyhit% <> 11% then L12150
            if keyhit% = 11% then       c% = c% + 1%
            if keyhit% = 11% then       insert_lines
            if keyhit% = 12% then gosub line_delete
            if keyhit% =  0% then       edit_line_item else L12150


        insert_lines   /* Insert Line Items per position C%  */
            if maxlines%  > ml% - 5% then end_insert
                gosub push_line
                gosub inputline
                     if keyhit% = 16% then end_insert
                          c% = c% + 1%
                          maxlines% = maxlines% + 1%
                          goto insert_lines
            end_insert
                gosub pop_line
                l% = max(0, maxlines% - 14%)    /* Last Screen */
                if maxlines% = 0% then check_header else line_summary

        inputline
            least%  = 3%
            for fieldnr% = 1% to 6%      /* First Line Item Screen     */
                gosub'053(fieldnr%)
                      if enabled% = 0 then L12710
L12590:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub restart_line
                      if keyhit% <>  4 then       L12680
L12630:                  fieldnr% = max(least%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% =     1% then L12590
                         if fieldnr% = least% then L12590
                         goto L12630
L12680:               if keyhit%  = 14 then gosub tb_inquiry
                      if keyhit%  = 16 then       return
                      if keyhit% <>  0 then       L12590
L12710:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L12590
            next fieldnr%
            return


        edit_line_item
            gosub describe_line

            edit_line
                lastfieldnr% = 0%
                gosub'103(0%, 2%)
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  =  6 then prev_line
                     if keyhit%  =  7 then next_line
                     if keyhit%  =  9 then       check_header
                     if keyhit%  = 12 then gosub delete_line
                     if keyhit%  = 14 then gosub tb_inquiry
                     if keyhit%  = 16 then       line_summary
                     if keyhit% <>  0 then       edit_line
L12910:         if pos("PA" = type$(c%)) > 0% then L12930
                if cursor%(2) < 50 then L12950
L12930:              fieldnr% = 6%
                     goto L13000
L12950:         fieldnr% = cursor%(1) - 13%
                     if fieldnr% < 3% then edit_line
                     if fieldnr% = 3% then L13000
                          fieldnr% = fieldnr% - 1%
                          if fieldnr% < 4% or fieldnr% > 5% then edit_line
L13000:              if fieldnr% = lastfieldnr% then edit_line
                gosub'053(fieldnr%)
                     if enabled% = 0% then       edit_line
L13030:         gosub'103(fieldnr%, 2%)
                     if keyhit%  =  1 then gosub startover
                     if keyhit% <>  0 then L13030
                gosub'153(fieldnr%)
                     if errormsg$ <> " " then L13030
                          lastfieldnr% = fieldnr%
                          goto L12910


        prev_line    /* Move back one line item    */
            c% = max(1%, c% - 1%)
            goto edit_line_item

        next_line    /* Advance to next line       */
            c% = min(c%+1%, maxlines%)
            goto edit_line_item

        line_delete  /* Delete line from summary screen                */
            gosub'112(c%)
            if keyhit% <> 16% then return
                gosub do_line_delete
                return clear all
                goto line_summary

        delete_line  /* Delete line being edited                       */
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE LINE",                          ~
                            "Press RETURN to cancel deletion", "-OR-",   ~
                            "Press PF-16 to DELETE line item.")
            if u3% <> 16% then return
                gosub do_line_delete
                c% = min(c%, maxlines%)
                return clear all
                goto edit_line_item

            do_line_delete
                gosub pop_line
                maxlines% = maxlines% - 1%
                if maxlines% > 0% then return
                     return clear
                     goto check_header


        describe_line     /* Describe line item C%                     */
            descr% = 99%

            descr_type:
                     on pos("PABUGSD" = type$(c%)) gosub L13550, L13560,   ~
                                                         L13570, L13580,   ~
                                                         L13590, L13600,   ~
                                                         L13610
                     goto L13620
L13550:                   typedescr$ = "Payment"            :  return
L13560:                   typedescr$ = "Adjustment"         :  return
L13570:                   typedescr$ = "Bal Fwd Payment"    :  return
L13580:                   typedescr$ = "Unapplied Payment"  :  return
L13590:                   typedescr$ = "G/L Distribution"   :  return
L13600:                   typedescr$ = "Direct Sale"        :  return
L13610:                   typedescr$ = "Bal Fwd Distr."     :  return
L13620:              typedescr$ = "(" & str(typedescr$,,18) & ")"
                     if descr% <> 99% then return

            descr_stlmnt:
                     postedmsg$ = "(Postings in Buffer)"
                     if other(c%) = 0 then postedmsg$ = " "
                     tcurrmsg$ = " "
                     if currency$(c%) <> currency$ then L13674
                     if currency$ = statutory$ then L13680
L13674:                 tcurrmsg$ = " (Currency:"
                        tcurrmsg$ = tcurrmsg$ & " " & currency$(c%) & ")"
                        if currency$(c%) <> currency$ then               ~
                                             str(tcurrmsg$,1,1) = hex(94)
L13680:              if descr% <> 99% then return

            descr_discp:
                     call "CONVERT" (discpct(c%), 2.2, discpct$)
                     if descr% <> 99% then return

            descr_due:
                     adjduef$  = adjdue$(c%)  : call "DATEFMT" (adjduef$)
                     duedatef$ = duedate$(c%) : call "DATEFMT" (duedatef$)
                     if duedatef$ <> " " then                            ~
                                duedatef$ = "(" & str(duedatef$,,8) & ")"
                     if descr% <> 99% then return

            descr_acct:
                     call "GLUNFMT" (cracct$(c%))
                     call "DESCRIBE" (#04, cracct$(c%), cracctdescr$, 1%,~
                                                                  f1%(4))
                     call "GLFMT" (cracct$(c%))
                     if descr% <> 99% then return

            descr_amts:
                     newbal = prev(c%) - (net(c%)-(disca(c%)+discu(c%)))
                     gross  = net(c%) - (disca(c%) + discu(c%))
                     call "CONVERT" (prev (c%), 2.2, curbal$)
                     call "CONVERT" (gross    , 2.2, gross$ )
                     call "CONVERT" (disca(c%), 2.2, disca$ )
                     call "CONVERT" (discu(c%), 2.2, discu$ )
                     call "CONVERT" (net  (c%), 2.2, net$   )
                     call "CONVERT" (newbal   , 2.2, newbal$)
                     if pos("GS" = type$(c%)) = 0% then L13990
                          curbal$, newbal$ = " "
L13990:              descr% = 0%
                     return

        push_line    /* Move lines at and below C% down one & clear C% */
            if c% > maxlines% then L14200
            for x% = maxlines% to c% step -1%
                type$     (x% + 1%) = type$     (x%)
                srcedoc$  (x% + 1%) = srcedoc$  (x%)
                srcetype$ (x% + 1%) = srcetype$ (x%)
                stlmnt$   (x% + 1%) = stlmnt$   (x%)
                po$       (x% + 1%) = po$       (x%)
                discpct   (x% + 1%) = discpct   (x%)
                adjdue$   (x% + 1%) = adjdue$   (x%)
                duedate$  (x% + 1%) = duedate$  (x%)
                net       (x% + 1%) = net       (x%)
                disca     (x% + 1%) = disca     (x%)
                discu     (x% + 1%) = discu     (x%)
                cracct$   (x% + 1%) = cracct$   (x%)
                prev      (x% + 1%) = prev      (x%)
                other     (x% + 1%) = other     (x%)
                currency$ (x% + 1%) = currency$ (x%)
                conveqv   (x% + 1%) = conveqv   (x%)
                convunt   (x% + 1%) = convunt   (x%)
                convdate$ (x% + 1%) = convdate$ (x%)
                conveqv1  (x% + 1%) = conveqv1  (x%)
                convunt1  (x% + 1%) = convunt1  (x%)
                convdate1$(x% + 1%) = convdate1$(x%)
            next x%
L14200:     gosub clear_line
            return

        pop_line    /* Move lines below C% up one & clear last line.   */
            if c%  > maxlines% then L14410
            for x% = c% to maxlines%
                type$     (x%) = type$     (x% + 1%)
                srcedoc$  (x%) = srcedoc$  (x% + 1%)
                srcetype$ (x%) = srcetype$ (x% + 1%)
                stlmnt$   (x%) = stlmnt$   (x% + 1%)
                po$       (x%) = po$       (x% + 1%)
                discpct   (x%) = discpct   (x% + 1%)
                adjdue$   (x%) = adjdue$   (x% + 1%)
                duedate$  (x%) = duedate$  (x% + 1%)
                net       (x%) = net       (x% + 1%)
                disca     (x%) = disca     (x% + 1%)
                discu     (x%) = discu     (x% + 1%)
                cracct$   (x%) = cracct$   (x% + 1%)
                prev      (x%) = prev      (x% + 1%)
                other     (x%) = other     (x% + 1%)
                currency$ (x%) = currency$ (x% + 1%)
                conveqv   (x%) = conveqv   (x% + 1%)
                convunt   (x%) = convunt   (x% + 1%)
                convdate$ (x%) = convdate$ (x% + 1%)
                conveqv1  (x%) = conveqv1  (x% + 1%)
                convunt1  (x%) = convunt1  (x% + 1%)
                convdate1$(x%) = convdate1$(x% + 1%)
            next x%
L14410:     gosub clear_last_line
            return

        clear_last_line   /* Clear all variables for MAXLINES% + 1%    */
            z% = maxlines% + 1%
            goto L14500

        clear_line   /* Clear all variables for line C%                */
            z% = c%
L14500:     init(" ") type$(z%), srcedoc$(z%), srcetype$(z%), po$(z%),   ~
                      stlmnt$(z%), adjdue$(z%), duedate$(z%),            ~
                      cracct$(z%), currency$(z%), convdate$(z%),         ~
                      convdate1$(z%)
            init(" ") typedescr$, postedmsg$, discpct$, adjduef$,        ~
                      duedatef$, cracctdescr$, curbal$, gross$, disca$,  ~
                      discu$, net$, newbal$, tcurrmsg$
            discpct(z%), net(z%), disca(z%), discu(z%), prev(z%),        ~
              other(z%)                                              = 0%
            conveqv(z%), conveqv1(z%), convunt(z%), convunt1(z%) = 1
            return

        tb_inquiry
            call "ARQTBPRM" (billto$, curr$, statutory$,                 ~
                             #06, #03, #02, #04)
            return

        REM *************************************************************~
            *      A U T O M A T I C   A P P L I C A T I O N S          *~
            *-----------------------------------------------------------*~
            * Allows user to automatically distribute payments to open  *~
            * items.                                                    *~
            *************************************************************
        auto_applications

*        Initialize and set standard defaults
            gosub auto_init
            gosub summarize_postings
            auto_amts(1,3)  = netchk - sum_net
            auto_amts$(1), auto_amts$(2) = "CALC"
            call "CONVERT" (auto_amts(1,3), 2.2, auto_amts$(3))

*        Allow user to monkey with the parameters.
        auto_parms
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       auto_abort
                  if keyhit%  =  9% then       auto_abort
                  if keyhit%  = 14% then gosub tb_inquiry
                  if keyhit%  = 16% then       apply_amounts
                  if keyhit% <>  0% then       auto_parms
L15130:     fieldnr% = cursor%(1) - 6%
            fieldnr% = max(1, min(fieldnr%, 7%))
            if fieldnr% = 7% then fieldnr% = 8%
            if fieldnr% = 6% and cursor%(2) > 40% then fieldnr% = 7%
            if fieldnr% = lastfieldnr% then auto_parms
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then auto_parms
L15155:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15155
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15155
                     lastfieldnr% = fieldnr%
                     goto L15130

        apply_amounts
            gosub auto_data_load   /* Sets up index and data */
            if auto_used% = 0% then auto_parms

          call "SHOSTAT" ("Applying Amounts")
            call "SORT" addr(str(auto_idx$()), auto_used%, 12%)
            if auto_amts$(2)= "CALC" then auto_amts(3,2) = -999999999999 ~
                                     else auto_amts(3,2) = auto_amts(1,2)
            auto_amts(3,3) = auto_amts(1,3)  /* Payment Limit */

            for a% = 1% to auto_used%
                convert str(auto_idx$(a%),9,4) to c%
                netleft = auto_amts(3,3) : discleft = auto_amts(3,2)
                if prev(c%) > 0 then L15290
                     if disca(c%) < discleft then disca(c%) = discleft
                     net(c%) = prev(c%) + disca(c%)
                     gross   = prev(c%)
                     goto L15330

L15290:         okdiscpct = 0
                if disca(c%) <> 0 then okdiscpct = discpct(c%) * .01
                net(c%)   = min(net(c%), netleft)
                if 1 - okdiscpct = 0 then gross = net(c%) else           ~
                             gross = round(net(c%) / (1 - okdiscpct), 2)
                disca(c%) = max(discleft, net(c%) - gross)
                gross     = net(c%) - disca(c%)

L15330:         if gross = 0 then L15365
                    str(auto_idx$(a%),,1) = "X"
                    auto_amts(2,1) = auto_amts(2,1) + gross
                    auto_amts(2,2) = auto_amts(2,2) + disca(c%)
                    auto_amts(2,3) = auto_amts(2,3) + net  (c%)
                    auto_amts(3,2) = auto_amts(3,2) - disca(c%)
                    auto_amts(3,3) = auto_amts(3,3) - net  (c%)
L15365:     next a%

            if force_balance$ = "N" then L15485
                if auto_amts$(2) = "CALC" then auto_amts(3,2) = 0
                if auto_amts(3,2) = 0 and auto_amts(3,3) = 0 then L15485
                     fnet  = auto_amts(3,3)
                     fdisc = auto_amts(3,2)
                     auto_used% = auto_used% + 1% : a% = auto_used%
                     c% = maxlines% + a%
                     str(auto_idx$(a%),,1) = "X"
                     convert c% to str(auto_idx$(a%),9,4), pic(0000)
                     prev(c%), other(c%) = 0
                     adjdue$(c%), duedate$(c%) = postmarku$
                     type$(c%)     = "U"
                     stlmnt$(c%)   = " "
                     srcedoc$(c%)  = chknr$
                     srcetype$(c%) = "CU"
                     po$(c%)       = " "
                     discpct(c%)   = 0
                     cracct$(c%)   = dflt_ar_acct$
                     disca(c%)     = fdisc
                     discu(c%)     = 0
                     net  (c%)     = fnet

L15485
*        Now set-up to display the results
*          CALL "DATEFMT" (AUTO_ASOF$)
            sum_disca      = sum_disca + auto_amts(2,2) + fdisc
            stat_sum_disca = stat_sum_disca +                            ~
                             round((auto_amts(2,2) + fdisc) * conveqv, 2)
            sum_net        = sum_net   + auto_amts(2,3) + fnet
            stat_sum_net   = stat_sum_net   +                            ~
                             round((auto_amts(2,3) + fnet) * conveqv, 2)
            gosub format_summary
            auto_left$(1), auto_left$(2) = "     -N/A-"
            if auto_amts$(1) = "CALC" and auto_amts$(2) = "CALC"         ~
                                                              then L15540
                call "CONVERT" (auto_amts(3,1), 2.2, auto_left$(1))
                call "CONVERT" (auto_amts(3,2), 2.2, auto_left$(2))

L15540:     inpmessage$ = "Press PF-12 to see Distribution Detail."
L15545:     gosub'104(9%, 3%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub auto_clear
                if keyhit%  =  1% then       auto_applications
                if keyhit%  = 12% then       auto_review
                if keyhit%  = 14% then gosub tb_inquiry
                if keyhit%  = 16% then       auto_save
                goto L15545

          auto_review     /* Review distribution details     */
            l1% = 0%
L15595:     gosub'105
                if keyhit%  =  0% then L15540
                if keyhit%  =  1% then gosub auto_clear
                if keyhit%  =  1% then auto_applications
                if keyhit%  =  2% then l1% = 0%
                if keyhit%  =  3% then l1% = auto_used% - 30%
                if keyhit%  =  4% then l1% = l1% - 30%
                if keyhit%  =  5% then l1% = l1% + 30%
                              l1% = max(0, min(l1%, auto_used%-30%))
                if keyhit%  = 14% then gosub tb_inquiry
                if keyhit%  = 16% then       auto_save
                goto L15595


        REM *************************************************************~
            *      C H E C K - O F F   A P P L I C A T I O N S          *~
            *-----------------------------------------------------------*~
            * Allows user to simply check-off items that have been paid *~
            * by check.                                                 *~
            *************************************************************
        check_off_routine

*        Initialize and set defaults
            gosub auto_init

*        Allow user to monkey with the parameters.
        check_off_parms
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'107(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       auto_abort
                  if keyhit%  =  9% then       auto_abort
                  if keyhit%  = 14% then gosub tb_inquiry
                  if keyhit%  = 16% then       load_check_off
                  if keyhit% <>  0% then       check_off_parms
L16220:     fieldnr% = cursor%(1) - 4%
            fieldnr% = max(2, min(fieldnr%, 6%))
            if fieldnr% = 6% then fieldnr% = 8%
            if fieldnr% = lastfieldnr% then check_off_parms
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then check_off_parms
L16290:     gosub'107(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L16290
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L16290
                     lastfieldnr% = fieldnr%
                     goto L16220

        load_check_off
            gosub auto_data_load   /* Sets up index and data */
            if auto_used% = 0% then check_off_parms

*        Now Display Items and let operator indicate payments.
            l1% = 0%
L16450:     gosub'106
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then l1% = 0%
                if keyhit%  =  3% then l1% = auto_used% - 30%
                if keyhit%  =  4% then l1% = l1% - 30%
                if keyhit%  =  5% then l1% = l1% + 30%
                              l1% = max(0, min(l1%, auto_used%-30%))
                if keyhit%  =  6% then gosub pay_all
                if keyhit%  =  7% then gosub unpay_all
                if keyhit%  =  8% then gosub display_amts
                if keyhit%  = 10% then       auto_abort
                if keyhit%  = 11% then       auto_abort
                if keyhit%  = 14% then gosub tb_inquiry
                if keyhit%  = 16% then       auto_save else L16450


        pay_all    :  paid% = 1%   :  goto L16630
        unpay_all  :  paid% = 0%
L16630:     for a% = 1% to auto_used%
                if paid% = 1% then str(auto_idx$(a%),,1) = "X" else      ~
                                   str(auto_idx$(a%),,1) = " "
            next a%
            return


        display_amts      /* Show summary total of items marked Paid   */
            sum_net, sum_disca = 0
            stat_sum_net, stat_sum_disca = 0
            for a% = 1% to auto_used%
                if str(auto_idx$(a%),,1) = " " then L16820
                     convert str(auto_idx$(a%),9,4) to c%
                     sum_net        = sum_net + net(c%)
                     stat_sum_net   = stat_sum_net +                     ~
                                      round(net(c%) * conveqv, 4)
                     sum_disca      = sum_disca + disca(c%)
                     stat_sum_disca = stat_sum_disca +                   ~
                                      round(disca(c%) * conveqv, 4)
L16820:     next a%

            sum_ar  = sum_net - sum_disca
            sum_ars = stat_sum_net - stat_sum_disca
            put inpmessage$ using L16920, sum_ar, sum_disca, sum_net
            call "ASKUSER" (2%, "CHECK-OFF SUMMARY",                     ~
                            "Checked Off items total as follows:",       ~
                            inpmessage$, "Press any PF key to return...")
            return

L16920: %A/R: -####,###.00   Discount: -####,###.00   Net: -####,###.00

        REM *************************************************************~
            *       A U T O M A T I C   S U B R O U T I N E S           *~
            *-----------------------------------------------------------*~
            * Support routines for both automatic and check-off appli-  *~
            * cation routines.                                          *~
            *************************************************************

        auto_init    /* Set up arrays and pointers and set maximum     */
                     /* number of lines that may be loaded.            */
            if ml% - maxlines% - 2% > 100% then L17120
                errormsg$ = "Insufficient room available to load items."
                return clear
L17120:     auto_used% = 0%
            init(" ") auto_idx$(), auto_ranges$(), auto_amts$(),         ~
                      auto_stlmnt$(), auto_acct$(), auto_acctie$
            mat auto_amts = zer
            fnet, fdisc   = 0
            auto_asof$      = date$
            auto_ranges$(1) = "D"
            auto_ranges$(2) = "ALL" : auto_ranges$(3) = " "
            auto_ranges$(4) = all(hex(00)):auto_ranges$(5) = all(hex(ff))
            auto_stlmnt$(1) = "ALL" : auto_stlmnt$(2) = " "
            auto_stlmnt$(3) = all(hex(00)):auto_stlmnt$(4) = all(hex(ff))
            auto_acctie$ = "I"
            auto_acct$(1) = "ALL" : auto_acct$(2) = " "
            auto_acct$(3) = "ALL" : auto_acct$(4) = " "
            credits_first$ = "Y"
            force_balance$ = "N"
            auto_currchk$  = "N"
            return


        auto_abort   /* Exit routine and return to specified spot      */
            return clear all
            gosub auto_clear
            a% = keyhit%  :  keyhit% = 99%
            if a% = 2% or a% = 11% then line_summary else check_header


        auto_data_load    /* Load candidates for payment               */
            call "SHOSTAT" ("Loading Trial Balance")
            auto_asofu$ = auto_asof$ : call "DATUNFMT" (auto_asofu$)
            plowkey$ = str(billto$) & str(auto_stlmnt$(3)) & hex(ff)

          auto_data_load_loop
            call "PLOWNEXT" (#06, plowkey$, 9%, f1%(6))
            if f1%(6) = 0% then exit_auto_data_load
            if str(plowkey$,10,8) > auto_stlmnt$(4) then                 ~
                                                     exit_auto_data_load

            if str(plowkey$,20,2) <> "00" then next_stlmnt
            if auto_currchk$ <> "Y" then L17480
               call "READ100" (#44, key(#06), f1%(44))
                  if f1%(44) <> 0% then L17466
                     if currency$ <> statutory$ then next_stlmnt
                        goto L17480
L17466:     if str(key(#44,1%),,4) <> currency$ then next_stlmnt

L17480:     if maxlines% = 0% then L17520
                search str(stlmnt$(),,maxlines%*12%) =                   ~
                                str(plowkey$,10,10) to cursor%() step 12
                if cursor%(1) <> 0% then next_stlmnt
L17520:     c% = maxlines% + auto_used% + 1%
            if c% < ml% - 2% then L17640
                u3% = 2%
                call "ASKUSER" (u3%, "OUT OF SPACE",                     ~
                       "Unable to load all open items for processing.",  ~
                       "Press RETURN to abort processing -OR-",          ~
                       "PF-16 to apply to items already loaded.")
                if u3% = 16% then exit_auto_data_load
L17600:              gosub auto_clear
                     auto_used% = 0%
                     return

L17640:     get #06 using L17670, discpct(c%), duedate$(c%), grace%,      ~
                                po$(c%), cracct$(c%), srcetype$(c%),     ~
                                srcedoc$(c%), posted$
L17670:         FMT POS(22), PD(14,4), CH(6), BI(1), XX(6), CH(16),      ~
                    POS(76), CH(9), XX(2), CH(2), CH(8), CH(6)
            call "DATE" addr("G+", duedate$(c%),  grace%, adjdue$(c%),   ~
                                                                     u3%)
            if auto_ranges$(1) = "D" and (adjdue$(c%) < auto_ranges$(4)  ~
                                      or  adjdue$(c%) > auto_ranges$(5)) ~
                                    then next_stlmnt
            if auto_ranges$(1) = "P" and (posted$     < auto_ranges$(4)  ~
                                      or  posted$     > auto_ranges$(5)) ~
                                    then next_stlmnt
            if pos("BU" = str(srcetype$(c%),2,1)) > 0%  then next_stlmnt
            if auto_acct$(1) = "ALL" then L17790
               if auto_acctie$ = "I" then L17776
                  if cracct$(c%) < auto_acct$(3) then L17790
                  if cracct$(c%) > auto_acct$(4) then L17790
L17775:              goto next_stlmnt
L17776:           if cracct$(c%) < auto_acct$(3) then L17775
                  if cracct$(c%) > auto_acct$(4) then L17775

L17790:     stlmnt$(c%) = str(plowkey$,10,10)
            call "GLFMT" (cracct$(c%))
            gosub call_armbalnc

            if bals(3) = 0 then next_stlmnt
                type$(c%) = "P"
                prev (c%) = bals(3)
                other(c%) = bals(2)
                net  (c%) = bals(3)
                disca(c%) = 0
                discu(c%) = 0
                if adjdue$(c%) >= postmarku$ then                        ~
                    disca(c%) = round(net(c%) * discpct(c%) * (-.01), 2)
                net  (c%) = net(c%) + disca(c%)

                auto_used% = auto_used% + 1%
                convert c% to str(auto_idx$(auto_used%),9,4), pic(0000)
                if credits_first$ = "Y" and bals(3) < 0 then             ~
                             str(auto_idx$(auto_used%),2,1) = hex(00)
                if auto_ranges$(1) = "P" then                            ~
                             str(auto_idx$(auto_used%),3,6) = posted$
                if auto_ranges$(1) = "D" then                            ~
                             str(auto_idx$(auto_used%),3,6) = adjdue$(c%)

              next_stlmnt
                str(plowkey$,20,2) = hex(ffff)
                goto auto_data_load_loop

            exit_auto_data_load
                if auto_used% > 0% then return
                     call "ASKUSER" (2%, "NO ITEMS LOADED",              ~
                       "No Items were found per the Selection Criteria.",~
                       "Press RETURN to Continue....", " ")
                     goto L17600


        auto_save    /* Put paid items onto check.                     */
            call "SHOSTAT" ("Adding Selected Items to Check...")
            adds% = 0%

*         First blank the Type for items that were not selected.
            for a% = 1% to auto_used%
                convert str(auto_idx$(a%),9,4) to c%
                if str(auto_idx$(a%),,1) = " " then type$(c%) = " "      ~
                                               else adds% = adds% + 1%
            next a%

*         Next Pop unselected items out of arrays.
            if adds% > 0% then L18300
                gosub auto_clear
                goto L18410
L18300:     c1% = maxlines% + 1%
            c2% = maxlines% + auto_used%
            maxlines% = maxlines% + auto_used%
L18330:     for c% = c1% to c2%
                if type$(c%) <> " " then L18390
                     maxlines% = maxlines% - 1%
                     gosub pop_line
                     c2% = c2% - 1%
                     goto L18330
L18390:     next c%
            gosub summarize_postings
L18410:     keyhit% = 99%
            return


        auto_clear   /* Clear arrays after abort or exit     */
            if auto_used% = 0% then return
                for c% = maxlines% + 1% to maxlines% + auto_used%
                     gosub clear_line
                next c%
                return


        call_armbalnc

            call "ARMCBLNC" (billto$, stlmnt$(c%), auto_asofu$, 10%,     ~
                            chknr$, #06, #10, sbals(), #44, #43, #45,    ~
                            currency$(c%), convdate$(c%), conveqv(c%),   ~
                            convunt(c%), cbals())

            if currency$(c%) <> " " then L18720
               currency$(c%) = statutory$:convdate1$(c%) = " "
               conveqv1(c%), convunt1(c%) = 1
               mat bals = sbals
               goto L18840

*          CURRENCY$(C%) = STATUTORY$:CONVDATE$(C%),CONVDATE1$(C%) = " "
*          CONVEQV(C%), CONVUNT(C%), CONVEQV1(C%), CONVUNT1(C%) = 1
*          READKEY$ = STR(BILLTO$) & STLMNT$(C%)
*          CALL "PLOWNEXT" (#44, READKEY$, 19%, F1%(44))
*             IF F1%(44) = 0% THEN 18840
*          GET #44 USING 18710, CURRENCY$(C%), CONVEQV(C%), CONVUNT(C%),~
*                               CONVDATE$(C%)
*              FMT CH(4), POS(34), 2*PD(14,7), CH(6)
L18720:     readkey$ = str(currtype$,,1) & str(currency$(c%),,4) &       ~
                       rev_date$
            convdate1$(c%) = convdate$(c%)
            conveqv1  (c%) = conveqv  (c%)
            convunt1  (c%) = convunt  (c%)
            call "PLOWNEXT" (#41, readkey$, 5%, f1%(41))
               if f1%(41) = 0% then L18801
            get #41 using L18800, convdate1$(c%),conveqv1(c%),convunt1(c%)
L18800:         FMT POS(12), CH(6), 2*PD(14,7)
L18801:     mat bals = cbals
            if currency$ = currency$(c%) then return
            for i% = 1% to 3%
                bals(i%) = conveqv1(c%) * bals(i%)
            next i%
L18840:         if currency$ <> statutory$ then L18845
            for i% = 1% to 3%
                bals(i%) = round(bals(i%), 4)
            next i%
            return
L18845:     for i% = 1% to 3%
                bals(i%) = round(convunt * bals(i%), 4)
            next i%
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        save_check
            if maxlines% > 0% then L19140
                u3% = 2%
                call "ASKUSER" (u3%, "DELETE CHECK?",                    ~
                          "This check will be deleted since there are",  ~
                          "no lines items on it.  Press RETURN to",      ~
                          "return to edit mode -or- PF-16 to Delete.")
                if u3% <> 16% then return
L19140:     return clear all
            gosub  dataput
            goto   remove_tasking


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20160,         /* Bill-to Number   */~
                                    L20200,         /* Check Number     */~
                                    L20240,         /* Check Date       */~
                                    L20280,         /* Bank Number      */~
                                    L20320,         /* Post Mark Date   */~
                                    L20370,         /* Net Check Amount */~
                                    L20430          /* Currency code    */
                     return

L20160
*        Bill-to Number                        BILLTO$
            inpmessage$ = "Enter Bill-to Customer Number."
            return

L20200
*        Check Number                          CHKNR$
            inpmessage$ = "Enter Check Number."
            return

L20240
*        Check Date                            CHKDATE$
            if maxlines% = 0% then L20250
            if currency$ <> statutory$ then enabled% = 0%
            search str(currency$(),,4*maxlines%) <> str(statutory$,,4)   ~
                   to temp%() step 4
            if temp%(1) <> 0% then enabled% = 0%
L20250:     inpmessage$ = "Enter Check Date."
            return

L20280
*        Bank Number                           BANKNR$
            inpmessage$ = "Enter Check's Bank Number."
            return

L20320
*        Post Mark Date                        POSTMARK$
            inpmessage$ = "Enter Post Mark Date."
            if postmark$ = " " then postmark$ = chkdate$
            return

L20370
*        Net Check Amount                      NETCHK$
            inpmessage$ = "Enter Net Check Amount."
            call "STRING" addr("LJ", netchk$, 10%)
            return

L20430
*        Currency code                         CURRENCY$
            if curr$ <> "Y" then enabled% = 0%
            if maxlines% > 0% then enabled% = 0%
            inpmessage$ = "Enter Currency Code or '?' to see list. Blan"&~
                "k = statutory."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   L I N E S       *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Line Input/Edit.     *~
            *************************************************************

        deffn'053(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L22160,               /* Posting Type     */~
                              L22210,               /* Settlement Nr    */~
                              L22300,               /* PO               */~
                              L22350,               /* Due Date         */~
                              L22410,               /* Credit Acct      */~
                              L22520                /* Amounts          */
            return

L22160
*        Posting Type
            inpmessage$ = "Enter Posting Type ('P'ayment, 'U'napplied, "&~
                          "'A'dj, 'B'alfwd, 'S'ale, 'G'/L)"
            type$(c%)   = "P"  :  gosub descr_type
            return

L22210
*        Settlement Number
            inpmessage$ = "Enter Settlement Number to apply against."
            if pos("BUGS" = type$(c%)) = 0% then return
                enabled% = 0%
                srcedoc$(c%)  = chknr$
                currency$(c%) = currency$
                conveqv(c%), conveqv1(c%) = conveqv
                convunt(c%), convunt1(c%) = convunt
                if currency$ = statutory$ then L22260
                   convdate$(c%), convdate1$(c%) = convdate$
L22260:         srcetype$(c%) = "C" & type$(c%)
                discpct(c%) = 0 : gosub descr_discp : gosub descr_stlmnt
                return

L22300
*        PO
            inpmessage$ = "Enter PO Number or Memo regarding Line Item."
            if pos("PA" = type$(c%)) > 0% then enabled% = 0%
            return

L22350
*        Due Date
            inpmessage$ = "Enter Due Date for Aging purposes."
            if pos("PAGS" = type$(c%)) > 0% then enabled% = 0%
            if adjdue$(c%) = " " then adjduef$ = postmark$
            return

L22410
*        Credit Account
            inpmessage$ = "Enter Credit G/L Account."
            if pos("PA" = type$(c%)) > 0% then enabled% = 0%
            if pos("PA" = type$(c%)) > 0% then return
            if cracct$(c%) <> " " then L22490
                cracct$(c%)  = dflt_ar_acct$
                if type$(c%) = "S" then cracct$(c%) = dflt_sales_acct$
                if type$(c%) = "G" then cracct$(c%) = " "
L22490:     gosub descr_acct
            return

L22520
*        Amounts
            inpmessage$ = "Enter Posting Amounts."
                call "STRING" addr("LJ", gross$, 10%)
                call "STRING" addr("LJ", disca$, 10%)
                call "STRING" addr("LJ", discu$, 10%)
                call "STRING" addr("LJ", net$  , 10%)
            return

        REM *************************************************************~
            *     E N A B L E   F O R   A U T O    A P P L I C .        *~
            *-----------------------------------------------------------*~
            * Sets Messages and Enables for Auot Application Screen.  . *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L23160,         /* Amounts          */~
                                    L23230,         /* As Of Date       */~
                                    L23270,         /* Settlement Range */~
                                    L23305,         /* Date Type/Range  */~
                                    L23327,         /* G/L Range        */~
                                    L23350,         /* Credits First?   */~
                                    L23390,         /* Force Balancing? */~
                                    L23440          /* Check Currency?  */
                     return

L23160
*        Amounts
            inpmessage$ = "Enter amounts to be applied."
            call "STRING" addr("LJ", auto_amts$(1), 10%)
            call "STRING" addr("LJ", auto_amts$(2), 10%)
            call "STRING" addr("LJ", auto_amts$(3), 10%)
            return

L23230
*        As Of Date
            inpmessage$ = "Enter Trial Balance As Of date."
            return

L23270
*        Range Selection
            inpmessage$ = "Enter Settlements Range" &                    ~
                          " (also ALL, FIRST, LAST)."
            return

L23305
*        Range Selection
            inpmessage$ = "Enter Code for Range type (' ', D or P" &     ~
                          ") and Date Ranges (also ALL, FIRST, LAST)"
            return

L23327
*        Selected Account
            inpmessage$ = "Enter G/L Account Range or 'ALL' & Include" & ~
                          "/Exclude selection (via I or E)."
            return

L23350
*        Process Credits First
            inpmessage$ = "Enter 'Y' to have credits processed first."
            return

L23390
*        Force Balancing?
            inpmessage$ = "Enter 'Y' to create an unapplied entry for" & ~
                          " any balance remaining."
            return

L23440
*        Check Currency only?
            inpmessage$ = "Enter 'Y' to process only transactions in"
            inpmessage$ = inpmessage$ & " " & currdesc$
            inpmessage$ = inpmessage$ & " (" & currency$ & ")"
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
        remove_tasking
                if tasking% = 0% then inputmode
                     readkey$ = str(billto$) & hex(0000000000000000)
                     call "DELETE" (#09, readkey$, 17%)
                     goto inputmode


        restart_line
            u3% = 2%
            call "ASKUSER" (u3%, "START LINE OVER?",                     ~
                     "Press RETURN to Start This Line Over",             ~
                     "-OR-",                                             ~
                     "Press PF-1 to Continue with entry.")
            if u3% = 1% then return
                return clear
                gosub  clear_line
                errormsg$ = " "
                goto inputline


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_check

            readkey$ = str(billto$) & chknr$
            call "READ100" (#09, readkey$, checkonfile%)
            if checkonfile% = 0% then L30760
                get #09 using L30110, errormsg$      /* Session ID */
L30110:              FMT POS(201), CH(6)
                if session$ = errormsg$ then L30160
                     errormsg$ = "Check Posted to Session " & errormsg$
                     return

L30160
*        Load Check from Buffer Files.
            currency$ = statutory$:conveqv, convunt = 1
            call "READ100" (#42, readkey$, f1%(42))
               if f1%(42) = 0% then L30220
            get #42 using L30210, currency$, convdate$, conveqv, convunt
L30210:         FMT CH(4), POS(22), CH(6), 2*PD(14,7)
L30220:     plowkey$ = str(billto$) & str(chknr$) & hex(00)
            call "PLOWNEXT" (#43, plowkey$, 17%, f1%(43))
               if f1%(43) = 0% then L30280
            get #43 using L30260, currency$, convdate$, conveqv, convunt
L30260:         FMT CH(4), POS(26), CH(6), 2*PD(14,7)
            call "GETCODE" (#40, currency$, currdesc$, 0%, 99, f1%(40))

L30280:     errormsg$ = " "
            print at(04,02), "Loading Check from Buffer File..."

            get #09 using L30320, chkdate$, banknr$, postmark$, netchk
L30320:         FMT POS(18), CH(6), CH(10), CH(6), PD(14,4)
            postmarku$ = postmark$
            call "DATEFMT" (chkdate$ )
            call "DATEFMT" (postmark$)
            call "READ100" (#42, key(#9), f1%(42))
               if f1%(42) = 0% then L30430
            get #42 using L30400, netchk
L30400:        FMT POS(44), PD(14,4)
            call "GETCODE" (#40, currency$, currdesc$, 0%, 99, f1%(40))
L30430:     call "CONVERT" (netchk, 2.2, netchk$)

            plowkey$ = str(billto$) & str(chknr$) & hex(00)
            c%, maxlines% = 0%
L30470:     call "PLOWNEXT" (#10, plowkey$, 17%, f1%(10))
            if f1%(10) = 0% then L30760
                c%, maxlines% = c% + 1%
                get #10 using L30550, type$(c%), stlmnt$(c%),             ~
                                     srcedoc$(c%), srcetype$(c%),        ~
                                     po$(c%), discpct(c%), adjdue$(c%),  ~
                                     duedate$(c%), net(c%), disca(c%),   ~
                                     discu(c%), cracct$(c%)
L30550:              FMT POS(22), CH(1), XX(12), CH(12), CH(8), CH(2),   ~
                         CH(16), PD(14,4), 2*CH(6), 3*PD(14,4), CH(9)
                call "GLFMT" (cracct$(c%))
                currency$(c%)=statutory$:convdate$(c%),convdate1$(c%)=" "
                conveqv(c%), convunt(c%), conveqv1(c%), convunt1(c%) = 1
                   call "READ100" (#43, key(#10), f1%(43))
                      if f1%(43) = 0% then L30611
                   get #43 using L30610, net(c%), disca(c%), discu(c%)
L30610:                FMT POS(48), 3*PD(14,4)
L30611:            call "READ100" (#45, key(#10), f1%(45))
                      if f1%(45) = 0% then L30690
                   get #45 using L30619, currency$(c%), convdate$(c%),    ~
                             conveqv(c%), convunt(c%), convdate1$(c%),   ~
                             conveqv1(c%), convunt1(c%)
L30619:                  FMT CH(4), POS(26), CH(6), 2*PD(14,7), CH(6),   ~
                             2*PD(14,7)
L30690:         if stlmnt$(c%) = " " then L30740
                     auto_asofu$ = date
                     gosub call_armbalnc
                     prev (c%) = bals(3)
                     other(c%) = bals(2)
L30740:         goto L30470

L30760:  /* Add up other postings already made to this bill-to.        */
            billto_other = 0
            billto_msg$  = " "
            plowkey$ = str(billto$) & hex(00)
L30800:     call "PLOWNEXT" (#09, plowkey$, 9%, f1%(9))
            if f1%(9) = 0% then L30900
                if str(plowkey$,10,8) = chknr$ then L30800
                     get #09 using L30840, net, disca, discu, gl
L30840:                   FMT POS(40), 4*PD(14,4)
                     billto_other =                                      ~
                               billto_other + net - (disca + discu) - gl
                     if gl <> 0 then                                     ~
                               billto_msg$ = "   (Direct Postings Exist)"
                goto L30800
L30900:     call "CONVERT" (billto_other, 2.2, billto_other$)
            net, disca, discu = 0
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

*        First remove check from buffer file. Done if no lines left.
            readkey$ = str(billto$) & chknr$
            call "DELETE" (#09, readkey$, 17%)
            call "DELETE" (#10, readkey$, 17%)
            call "DELETE" (#42, readkey$, 17%)
            call "DELETE" (#43, readkey$, 17%)
            call "DELETE" (#45, readkey$, 17%)

            if maxlines% = 0% then return

*        Now write lines
            for c% = 1% to maxlines%
                call "GLUNFMT" (cracct$(c%))
                convert c% to seq$, pic(0000)
                if currency$(c%) <> currency$ then L31230
                if currency$(c%) = statutory$ then L31420
                   /* If things blow up at the following divide, then */
                   /* there are serious problems all the way back to  */
                   /* CURFACTR letting a record with zero be written  */
L31230:            bals(1) = round((conveqv/conveqv1(c%)) * net(c%)  , 4)
                   bals(2) = round((conveqv/conveqv1(c%)) * disca(c%), 4)
                   bals(3) = round((conveqv/conveqv1(c%)) * discu(c%), 4)

                   write #45 using L31300, currency$(c%), billto$, chknr$,~
                             seq$, convdate$(c%), conveqv(c%),           ~
                             convunt(c%), convdate1$(c%), conveqv1(c%),  ~
                             convunt1(c%), bals(), " "
L31300:                  FMT CH(4), CH(9), CH(8), CH(4), CH(6),          ~
                             2*PD(14,7), CH(6), 2*PD(14,7), 3*PD(14,4),  ~
                             CH(7)
                if currency$ = statutory$ then L31420
                   write #43 using L31370, currency$, billto$, chknr$,    ~
                             seq$, convdate$, conveqv, convunt,          ~
                             net(c%), disca(c%), discu(c%), " "
L31370:                  FMT CH(4), CH(9), CH(8), CH(4), CH(6),          ~
                             2*PD(14,7), 3*PD(14,4), CH(29)
                   net  (c%) = round(net  (c%) * conveqv, 4)
                   disca(c%) = round(disca(c%) * conveqv, 4)
                   discu(c%) = round(discu(c%) * conveqv, 4)
L31420:         write #10 using L31470, billto$, chknr$, seq$, type$(c%), ~
                    " ", stlmnt$(c%), srcedoc$(c%), srcetype$(c%),       ~
                    po$(c%), discpct(c%), adjdue$(c%), duedate$(c%),     ~
                    net(c%), disca(c%), discu(c%), cracct$(c%), " ",     ~
                    billto$, stlmnt$(c%), chknr$, seq$, " "
L31470:              FMT CH(9), CH(8), CH(4), CH(1), 2*CH(12), CH(8),    ~
                         CH(2), CH(16), PD(14,4), 2*CH(6), 3*PD(14,4),   ~
                         CH(9), CH(75), CH(9), CH(12), CH(8),CH(4),CH(67)

            next c%

*        And now write Header to buffer
            call "DATUNFMT" (chkdate$)
            if currency$ = statutory$ then L31640
               write #42 using L31580, currency$, billto$, chknr$,       ~
                         convdate$, conveqv, convunt,                    ~
                         netchk, sum_disca, sum_discu, sum_gl, " "
L31580:                  FMT CH(4), CH(9), CH(8), CH(6), 2*PD(14,7),     ~
                             4*PD(14,4), CH(25)
               netchk    = stat_sum_net
               sum_disca = stat_sum_disca
               sum_discu = stat_sum_discu
               sum_gl    = stat_sum_gl
L31640:     write #09 using L31680, billto$, chknr$, chkdate$, banknr$,   ~
                postmarku$, netchk, sum_disca, sum_discu, sum_gl, " ",   ~
                userid$, date, " ", session$, billto$, chknr$, " "

L31680:         FMT CH(9), CH(8), CH(6), CH(10), CH(6), 4*PD(14,4),      ~
                    CH(27), CH(3), CH(6), CH(93), CH(6), CH(9), CH(8),   ~
                    CH(77)

            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()  else            ~
                                  init(hex(86)) lfac$()
            on fieldnr% gosub L40105,               /* Bill-to Number   */~
                              L40105,               /* Check Number     */~
                              L40105,               /* Check Date       */~
                              L40105,               /* Bank Number      */~
                              L40105,               /* Post Mark Date   */~
                              L40120,               /* Net Check Amount */~
                              L40105                /* Currency code    */
            goto L40140

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40105:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40120:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40140:     accept                                                       ~
               at (01,02), "Cash Receipts Entry",                        ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bill-to Number",                             ~
               at (06,30), fac(lfac$( 1)), billto$              , ch(09),~
               at (06,49), fac(hex(8c)),   billto_name$         , ch(32),~
                                                                         ~
               at (07,02), "Check Number",                               ~
               at (07,30), fac(lfac$( 2)), chknr$               , ch(08),~
                                                                         ~
               at (08,02), "Check Date",                                 ~
               at (08,30), fac(lfac$( 3)), chkdate$             , ch(08),~
                                                                         ~
               at (09,02), "Bank Number",                                ~
               at (09,30), fac(lfac$( 4)), banknr$              , ch(10),~
                                                                         ~
               at (10,02), "Post Mark Date",                             ~
               at (10,30), fac(lfac$( 5)), postmark$            , ch(08),~
                                                                         ~
               at (11,02), "Net Check Amount",                           ~
               at (11,30), fac(lfac$( 6)), netchk$              , ch(10),~
                                                                         ~
               at (12,02), "Currency code",                              ~
               at (12,30), fac(lfac$( 7)), currency$            , ch(04),~
               at (12,49), fac(hex(8c)),   currdesc$            , ch(32),~
                                                                         ~
               at (14,02), "Posting Summary -",                          ~
               at (14,20),  fac(hex(8c)), currency$             , ch(04),~
               at (15,04), "Accounts Receivable",                        ~
               at (15,26),  fac(hex(8c)),  sum_ar$              , ch(12),~
               at (16,04),  "Allowed Discounts",                         ~
               at (16,26),  fac(hex(8c)),  sum_disca$           , ch(12),~
               at (17,04),  "Unallowed Discounts",                       ~
               at (17,26),  fac(hex(8c)),  sum_discu$           , ch(12),~
               at (18,04),  "Direct Sales & G/L",                        ~
               at (18,26),  fac(hex(ac)),  sum_gl$              , ch(12),~
               at (19,04),  "** Net Amount",                             ~
               at (19,26),  fac(hex(8c)),  sum_net$             , ch(12),~
                                                                         ~
               at (14,47), "Bill-to Balances -",                         ~
               at (14,66),  fac(hex(8c)),  statutory$           , ch(04),~
               at (15,49), "Open A/R",                                   ~
               at (15,69),  fac(hex(8c)),  billto_ar$           , ch(12),~
               at (16,49),  "Postings This Check",                       ~
               at (16,69),  fac(hex(8c)),  sum_ars$             , ch(12),~
               at (17,49),  "Other Postings     ",                       ~
               at (17,69),  fac(hex(ac)),  billto_other$        , ch(12),~
               at (18,49),  "** New Balance",                            ~
               at (18,68),  fac(hex(8c)),  billto_bal$          , ch(13),~
               at (19,49),  fac(hex(8c)),  billto_msg$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 13 then L40435  :  call "MANUAL" ("CRCINPUT")
                                            goto L40140
L40435:     if keyhit% <> 15 then L40445  :  call "PRNTSCRN"
                                            goto L40140
L40445:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return


        set_pf1
        if edit% = 2% then L40545         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0f10ffffff00)
           if fieldnr%  = 1% then L40525
                str(pf$(3),64) = " "     :  str(pfkey$,16,1) = hex(ff)
L40525:    if fieldnr%  > 3% then L40535
                str(pf$(2),18,26) = " "  :  str(pfkey$, 4,1) = hex(ff)
L40535:    return

L40545:  if fieldnr% > 0% then L40610     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items  (8)Automatic Application  (12)Delet"&~
                    "e Check      (15)Print Screen"
           pf$(3) = "               (9)Check-Off Application  (14)Trial"&~
                    " Balance     (16)Save Check  "
           pfkey$ = hex(0102ffffffffff0809ffff0c0d0e0f10ffffff00)
           if netchk = sum_net then L40595
                str(pf$(3),64) = " "  :  str(pfkey$,16,1) = hex(ff)
L40595:    return

                                         /* Edit Mode- Field Enabled   */
L40610:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen.                                 *~
            *************************************************************

        deffn'102                        /* Display and Select Line    */
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            edit% = 1%
            window% = 15%  :  gosub set_summary_window
            gosub set_pf2a
            goto L41105

        deffn'112(c%)                    /* Delete Line Item           */
            edit% = 2%
            init (hex(8c)) sfac$()
            sfac$(c% - l%) = hex(94)
            gosub set_pf2b
            goto L41105

L41105:     accept                                                       ~
               at (01,02), "Cash Receipts Entry- Line Item Summary",     ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (04,07), fac(hex(ac)), hdr$(2)                , ch(04),~
               at (04,12), fac(hex(ac)), hdr$(3)                , ch(11),~
               at (04,24), fac(hex(ac)), hdr$(4)                , ch(11),~
               at (04,36), fac(hex(ac)), hdr$(5)                , ch(12),~
               at (04,49), fac(hex(ac)), hdr$(6)                , ch(10),~
               at (04,60), fac(hex(ac)), hdr$(7)                , ch(10),~
               at (04,71), fac(hex(ac)), hdr$(8)                , ch(10),~
                                                                         ~
               at (05,02), fac(hex(80))  , summary$( 1%)        , ch(79),~
               at (05,02), fac(sfac$( 1)), summary$( 1%)        , ch(79),~
               at (06,02), fac(sfac$( 2)), summary$( 2%)        , ch(79),~
               at (07,02), fac(sfac$( 3)), summary$( 3%)        , ch(79),~
               at (08,02), fac(sfac$( 4)), summary$( 4%)        , ch(79),~
               at (09,02), fac(sfac$( 5)), summary$( 5%)        , ch(79),~
               at (10,02), fac(sfac$( 6)), summary$( 6%)        , ch(79),~
               at (11,02), fac(sfac$( 7)), summary$( 7%)        , ch(79),~
               at (12,02), fac(sfac$( 8)), summary$( 8%)        , ch(79),~
               at (13,02), fac(sfac$( 9)), summary$( 9%)        , ch(79),~
               at (14,02), fac(sfac$(10)), summary$(10%)        , ch(79),~
               at (15,02), fac(sfac$(11)), summary$(11%)        , ch(79),~
               at (16,02), fac(sfac$(12)), summary$(12%)        , ch(79),~
               at (17,02), fac(sfac$(13)), summary$(13%)        , ch(79),~
               at (18,02), fac(sfac$(14)), summary$(14%)        , ch(79),~
               at (19,02), fac(sfac$(15)), summary$(15%)        , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L41335
                     call "MANUAL" ("CRCINPUT")
                     goto L41105

L41335:        if keyhit% <> 15% then L41355
                     call "PRNTSCRN"
                     goto L41105

L41355:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2a                         /* Display Mode               */
           inpmessage$ = "To Modify a Line Item, Position Cursor and"  & ~
                          " press RETURN."
           pf$(1) = "(1)Start Check Over      (2)First (5)Next   (11)In"&~
                    "sert Lines   (13)Instructions"
           pf$(2) = "(8)Automatic Application (3)Last  (6)Down   (12)De"&~
                    "lete Line    (15)Print Screen"
           pf$(3) = "(9)Check-Off Application (4)Prev  (7)Up     (14)Tr"&~
                    "ial Balance  (16)Check Header"
           pfkey$ = hex(010203040506070809ff0b0c0d0e0f10ffffff00)
           if l% <> 0% then L41435
                str(pf$(1),26,8), str(pf$(3),26,8), str(pf$(2),35,7) = " "
                str(pfkey$, 2,1), str(pfkey$, 4,1), str(pfkey$, 6,1)     ~
                                                              = hex(ff)
L41435:    if l% + 15% < maxlines% then L41455
                str(pf$(2),26,8), str(pf$(1),35,7), str(pf$(3),35,7) = " "
                str(pfkey$, 3,1), str(pfkey$, 5,1), str(pfkey$, 7,1)     ~
                                                              = hex(ff)
L41455:    return

        set_pf2b                         /* Delete Line Item           */
           inpmessage$ = "Press PF-16 to delete line, PF-1 to exit."
           pf$(1) = "( 1)EXIT Delete                                   "&~
                    "             (13)Instructions"
           pf$(2) = "(16)DELETE Line Item                              "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0f10ffffffff)
           return


        set_summary_window  /* Set-up Summary Window starting w/ L%-1  */
            init (hex(86)) sfac$()
            if window% < 15% then init (hex(8c)) sfac$()
            init(" ") summary$()
            for dl% = 1% to window%  /* DL% = Screen Line, CL% = Check */
              cl% = dl% + l% : if type$(cl%) = " " then return
                convert cl% to str(summary$(dl%),,4), pic(###0)
                str(summary$(dl%), 7) = type$      (cl%)
                if currency$(cl%) <> currency$ then                      ~
                                           str(summary$(dl%), 8,1) = "*"
                if stlmnt$(cl%) = " " then L41575
                str(summary$(dl%),11) = str(stlmnt$(cl%),1,8) & "-" &    ~
                                        str(stlmnt$(cl%),9,2)
L41575:         str(summary$(dl%),23) = srcedoc$   (cl%)
                str(summary$(dl%),32) = srcetype$  (cl%)
                str(summary$(dl%),35) = cracct$    (cl%)
                call "CONVERT" (prev(cl%), 2.2, str(summary$(dl%),48,10))
                temp = net(cl%) - (disca(cl%) + discu(cl%))
                call "CONVERT" (temp     , 2.2, str(summary$(dl%),59,10))
                temp = prev(cl%) - temp
                call "CONVERT" (temp     , 2.2, str(summary$(dl%),70,10))
                if pos("GS" = type$(cl%)) = 0% then L41630
                     str(summary$(dl%),48,10) = " "
                     str(summary$(dl%),70,10) = " "
L41630:     next dl%
            return


        REM *************************************************************~
            *       L I N E   I T E M   I N P U T  /  E D I T           *~
            * --------------------------------------------------------- *~
            * Allows direct entry or editing of distribution line C%.   *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            l% = max(0%, c% - 4%)
            window% = 7% : gosub set_summary_window : sfac$(7) = hex(ac)
            sfac$(c%-l%) = hex(84) : if c%-l% = 7% then sfac$(7) = hex(a4)
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            gosub set_pf3
            if fieldnr% > 0% then init(hex(8c)) lfac$()  else            ~
                                  init(hex(86)) lfac$()
            if fieldnr% = 0% then inpmessage$ = edtmessage$
            on fieldnr% gosub L42165,               /* Posting Type     */~
                              L42165,               /* Settlement Nr    */~
                              L42150,               /* PO               */~
                              L42165,               /* Due Date         */~
                              L42165,               /* Credit Acct      */~
                              L42180                /* Amounts          */
                lfac$(8) = lfac$(6)
                if fieldnr% = 6% and pos("GS" = type$(c%)) > 0% then     ~
                                                      lfac$(6) = hex(8c)
                lfac$(7) = lfac$(6) or hex(20)   /* Underline */
            goto L42205

L42150:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L42165:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L42180:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return


L42205:     accept                                                       ~
               at (01,02), "Cash Receipts Entry- Line Item Entry/Edit",  ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (04,07), fac(hex(ac)), hdr$(2)                , ch(04),~
               at (04,12), fac(hex(ac)), hdr$(3)                , ch(11),~
               at (04,24), fac(hex(ac)), hdr$(4)                , ch(11),~
               at (04,36), fac(hex(ac)), hdr$(5)                , ch(12),~
               at (04,49), fac(hex(ac)), hdr$(6)                , ch(10),~
               at (04,60), fac(hex(ac)), hdr$(7)                , ch(10),~
               at (04,71), fac(hex(ac)), hdr$(8)                , ch(10),~
                                                                         ~
               at (05,02), fac(sfac$( 1)), summary$( 1%)        , ch(79),~
               at (06,02), fac(sfac$( 2)), summary$( 2%)        , ch(79),~
               at (07,02), fac(sfac$( 3)), summary$( 3%)        , ch(79),~
               at (08,02), fac(sfac$( 4)), summary$( 4%)        , ch(79),~
               at (09,02), fac(sfac$( 5)), summary$( 5%)        , ch(79),~
               at (10,02), fac(sfac$( 6)), summary$( 6%)        , ch(79),~
               at (11,02), fac(sfac$( 7)), summary$( 7%)        , ch(79),~
                                                                         ~
               at (12,02), fac(hex(94))  , errormsg$            , ch(79),~
                                                                         ~
               at (13,02), "Posting Type:",                              ~
               at (13,16), fac(lfac$( 1)), type$(c%)            , ch(01),~
               at (13,30), fac(hex(8c))  , typedescr$           , ch(20),~
                                                                         ~
               at (14,02), "Settlement #:",                              ~
               at (14,16), fac(lfac$( 2)), str(stlmnt$(c%),1,8) , ch(08),~
               at (14,25), fac(lfac$( 2)), str(stlmnt$(c%),9,2) , ch(02),~
               at (14,30), fac(hex(84))  , postedmsg$           , ch(20),~
                                                                         ~
               at (15,02), "  Source Doc:",                              ~
               at (15,16), fac(hex(8c))  , srcedoc$(c%)         , ch(08),~
               at (15,25), fac(hex(8c))  , srcetype$(c%)        , ch(02),~
               at (15,29), fac(hex(84))  , tcurrmsg$            , ch(21),~
                                                                         ~
               at (16,02), "Customer PO#:",                              ~
               at (16,16), fac(lfac$( 3)), po$(c%)              , ch(16),~
                                                                         ~
               at (17,02), "Cash Disc % :",                              ~
               at (17,16), fac(hex(8c)),   discpct$             , ch(06),~
                                                                         ~
               at (18,02), "Adj Due Date:",                              ~
               at (18,16), fac(lfac$( 4)), adjduef$             , ch(08),~
               at (18,30), fac(hex(8c))  , duedatef$            , ch(10),~
                                                                         ~
               at (19,02), "Credit Acct :",                              ~
               at (19,16), fac(lfac$( 5)), cracct$(c%)          , ch(12),~
               at (19,30), fac(hex(8c))  , cracctdescr$         , ch(32),~
                                                                         ~
               at (13,53), "Current Balance",                            ~
               at (13,71), fac(hex(8c))  , curbal$              , ch(10),~
               at (14,53), "Gross Payment  ",                            ~
               at (14,71), fac(lfac$( 6)), gross$               , ch(10),~
               at (15,53), "Allowed Discounts",                          ~
               at (15,71), fac(lfac$( 6)), disca$               , ch(10),~
               at (16,53), "Unallowed Discnts",                          ~
               at (16,71), fac(lfac$( 7)), discu$               , ch(10),~
               at (17,53), "Net Payment Amt  ",                          ~
               at (17,71), fac(lfac$( 8)), net$                 , ch(10),~
               at (18,53), "New Balance      ",                          ~
               at (18,70), fac(hex(8c))  , newbal$              , ch(11),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L42595
                     call "MANUAL" ("CRCINPUT")
                     goto L42205

L42595:        if keyhit% <> 15% then L42615
                     call "PRNTSCRN"
                     goto L42205

L42615:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3   :   if edit% = 2% then L42710
*        Input Mode
           pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Start Line Over       (4)Previous Field        "&~
                    "             (15)Print Screen"
           pf$(3) = "                                            (14)Tr"&~
                    "ial Balance  (16)Exit Inserts"
           pfkey$ = hex(0102ff04ffffffffffffffff0d0e0f10ffffff00)
           if fieldnr% = 1% then L42690
                str(pf$(3),64) = " " :  str(pfkey$,16,1) = hex(ff)
L42690:    if fieldnr% > least% then L42700
                str(pf$(2),25,20) = " " :  str(pfkey$,4,1) = hex(ff)
L42700:    return

L42710:  if fieldnr% > 0% then L42785
*        Edit Mode- Display
           pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "(6)Prev Line           (9)Check Header      (12)De"&~
                    "lete Line    (15)Print Screen"
           pf$(3) = "(7)Next Line                                (14)Tr"&~
                    "ial Balance  (16)Line Summary"
           pfkey$ = hex(01ffffffff0607ff09ffff0c0d0e0f10ffffff00)
           if c% > 1% then L42765
                str(pf$(2),,12) = " "  :   str(pfkey$,6,1) = hex(ff)
L42765:    if c% < maxlines% then L42775
                str(pf$(3),,12) = " "  :   str(pfkey$,7,1) = hex(ff)
L42775:    return

L42785
*        Edit Mode- Modify Field
           pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *   A U T O   A P P L I C A T I O N   S C R E E N   1       *~
            *-----------------------------------------------------------*~
            * Allows change of parameters and display of results.       *~
            *************************************************************

        deffn'104(fieldnr%, edit%)  /* 2% = Edit, 3% = Display         */
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            auto_hdrs$(1) = "Apply Amts"
            auto_hdrs$(2) = "   Applied"
            auto_hdrs$(3) = " Remaining"
            if force_balance$ = "Y" then auto_hdrs$(3) = "Amt Forced"
            gosub set_pf4
            init(hex(86)) lfac$()  :  init(hex(9c)) sfac$()
            if fieldnr% > 0% then init(hex(8c)) lfac$()
            if edit% = 3%    then lfac$(1) = hex(84)
            if edit% = 3%    then sfac$(1) = hex(84)
            if edit% = 3%    then sfac$(2) = hex(ac)
            on fieldnr% gosub L43150,               /* Amounts          */~
                              L43150,               /* As Of Date       */~
                              L43150,               /* Stlmnt# Range    */~
                              L43150,               /* Date Type/Range  */~
                              L43150,               /* G/L Range        */~
                              L43150,               /* Credits First?   */~
                              L43150,               /* Force Balance?   */~
                              L43150                /* Currency?        */
            goto L43185

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L43150:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L43185:     accept                                                       ~
               at (01,02), "Cash Receipts Entry- Auto Application",      ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,30), fac(sfac$( 2)), auto_hdrs$(1)        , ch(10),~
               at (05,02), "Apply Amts- Gross    ",                      ~
               at (05,30), fac(lfac$( 1)), auto_amts$(1)        , ch(10),~
               at (06,02), "            Cash Discs",                     ~
               at (06,30), fac(lfac$( 1)), auto_amts$(2)        , ch(10),~
               at (07,02), "            Net Paymnt",                     ~
               at (07,30), fac(lfac$( 1)), auto_amts$(3)        , ch(10),~
                                                                         ~
               at (04,44), fac(sfac$( 2)), auto_hdrs$(2)        , ch(10),~
               at (05,44), fac(sfac$(1)),auto_amts(2,1),pic(-#######.##),~
               at (06,44), fac(sfac$(1)),auto_amts(2,2),pic(-#######.##),~
               at (07,44), fac(sfac$(1)),auto_amts(2,3),pic(-#######.##),~
                                                                         ~
               at (04,58), fac(sfac$( 2)), auto_hdrs$(3)        , ch(10),~
               at (05,58), fac(sfac$(1)), auto_left$(1)         , ch(10),~
               at (06,58), fac(sfac$(1)), auto_left$(2)         , ch(10),~
               at (07,58), fac(sfac$(1)),auto_amts(3,3),pic(-#######.##),~
                                                                         ~
               at (08,02), "As Of Date",                                 ~
               at (08,30), fac(lfac$( 2)), auto_asof$           , ch(08),~
                                                                         ~
               at (09,02), "Settlement # Range:",                        ~
               at (09,30), fac(lfac$( 3)), auto_stlmnt$(1)      , ch(08),~
               at (09,43), "to",                                         ~
               at (09,46), fac(lfac$( 3)), auto_stlmnt$(2)      , ch(08),~
                                                                         ~
               at (10,02), "Date Type (P/D)/Range:",                     ~
               at (10,27), fac(lfac$( 4)), auto_ranges$(1)      , ch(01),~
               at (10,30), fac(lfac$( 4)), auto_ranges$(2)      , ch(08),~
               at (10,43), "to",                                         ~
               at (10,46), fac(lfac$( 4)), auto_ranges$(3)      , ch(08),~
                                                                         ~
               at (11,02), "Include/Exclude G/L Accts:",                 ~
               at (11,30), fac(lfac$( 5)), auto_acct$(1)        , ch(12),~
               at (11,43), "to",                                         ~
               at (11,46), fac(lfac$( 5)), auto_acct$(2)        , ch(12),~
               at (11,60), "Include/Exclude",                            ~
               at (11,76), fac(lfac$( 5)), auto_acctie$         , ch(01),~
                                                                         ~
               at (12,02), "Process Credits First?",                     ~
               at (12,30), fac(lfac$( 6)), credits_first$       , ch(01),~
                                                                         ~
               at (12,42), "Force Balancing?",                           ~
               at (12,70), fac(lfac$( 7)), force_balance$       , ch(01),~
                                                                         ~
               at (13,02), "Check Currency Only?",                       ~
               at (13,30), fac(lfac$( 8)), auto_currchk$        , ch(01),~
                                                                         ~
               at (14,02), "Posting Summary -",                          ~
               at (14,20),  fac(hex(8c)), currency$             , ch(04),~
               at (15,04), "Accounts Receivable",                        ~
               at (15,26),  fac(hex(8c)),  sum_ar$              , ch(12),~
               at (16,04),  "Allowed Discounts",                         ~
               at (16,26),  fac(hex(8c)),  sum_disca$           , ch(12),~
               at (17,04),  "Unallowed Discounts",                       ~
               at (17,26),  fac(hex(8c)),  sum_discu$           , ch(12),~
               at (18,04),  "Direct Sales & G/L",                        ~
               at (18,26),  fac(hex(ac)),  sum_gl$              , ch(12),~
               at (19,04),  "** Net Amount",                             ~
               at (19,26),  fac(hex(8c)),  sum_net$             , ch(12),~
                                                                         ~
               at (14,47), "Bill-to Balances -",                         ~
               at (14,66),  fac(hex(8c)),  statutory$           , ch(04),~
               at (15,49), "Open A/R",                                   ~
               at (15,69),  fac(hex(8c)),  billto_ar$           , ch(12),~
               at (16,49),  "Postings This Check",                       ~
               at (16,69),  fac(hex(8c)),  sum_ars$             , ch(12),~
               at (17,49),  "Other Postings     ",                       ~
               at (17,69),  fac(hex(ac)),  billto_other$        , ch(12),~
               at (18,49),  "** New Balance",                            ~
               at (18,68),  fac(hex(8c)),  billto_bal$          , ch(13),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 13 then L43550  :  call "MANUAL" ("CRCINPUT")
                                            goto L43185
L43550:     if keyhit% <> 15 then L43560  :  call "PRNTSCRN"
                                            goto L43185
L43560:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return


        set_pf4
        if edit% = 2% then L43640         /* Display Mode               */
           pf$(1) = "( 1)Start Distribution Over (Don't Accept)        "&~
                    "             (13)Instructions"
           pf$(2) = "(12)Review Distributions Created                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (14)Trial"&~
                    " Balance     (16)Accept Distr"
           pfkey$ = hex(01ffffffffffffffffffff0c0d0e0f10ffffff00)
           return

L43640:  if fieldnr% > 0% then L43695     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                                     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Check Header                          (14)Trial"&~
                    " Balance     (16)Apply Amts  "
           pfkey$ = hex(0102ffffffffffff09ffffff0d0e0f10ffffff00)
           return

                                         /* Edit Mode- Field Enabled   */
L43695:    pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *          A U T O   D I S T R .   R E V I E W              *~
            * --------------------------------------------------------- *~
            * Review Details of Auto Distribution.                      *~
            *************************************************************

        deffn'105
            gosub set_pf5
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            auto_hdrs$(1) = "Settlement"
            auto_hdrs$(2) = "   Payment"
            auto_hdrs$(3) = " Cash Disc"
            auto_hdrs$(4) = "T/C"
            inpmessage$  = "Press RETURN to return to Main Screen."
            init(hex(84)) sfac$()
            init(" ") summary$()
            for dl% = 1% to 30%
              i% =  dl% + l1% : if i% > auto_used% then L44116
                convert str(auto_idx$(dl%),9,4) to c%
                if net(c%) = 0 and disca(c%)= 0 then sfac$(dl%) = hex(8c)
                summary$(dl%) = stlmnt$(c%)
                str(summary$(dl%),11,6) =                                ~
                             sfac$(dl%) & str(currency$(c%)) & sfac$(dl%)
                if currency$(c%) =  currency$ then                       ~
                             str(summary$(dl%),11,1) = xor hex(08)
                call "CONVERT" (net(c%)  , 2.2, str(summary$(dl%),17,10))
                call "CONVERT" (disca(c%), 2.2, str(summary$(dl%),28,10))
                if disca(c%) = 0 then str(summary$(dl%),28,10) = " "
L44116:     next dl%

L44125:     accept                                                       ~
               at (01,02),                                               ~
                     "Cash Receipts Entry- Auto Application Review",     ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), auto_hdrs$(1)          , ch(10),~
               at (04,13), fac(hex(ac)), auto_hdrs$(4)          , ch(04),~
               at (04,18), fac(hex(ac)), auto_hdrs$(2)          , ch(10),~
               at (04,29), fac(hex(ac)), auto_hdrs$(3)          , ch(10),~
                                                                         ~
               at (04,42), fac(hex(ac)), auto_hdrs$(1)          , ch(10),~
               at (04,53), fac(hex(ac)), auto_hdrs$(4)          , ch(04),~
               at (04,58), fac(hex(ac)), auto_hdrs$(2)          , ch(10),~
               at (04,69), fac(hex(ac)), auto_hdrs$(3)          , ch(10),~
                                                                         ~
               at (05,02), fac(sfac$( 1)), summary$( 1%)        , ch(39),~
               at (06,02), fac(sfac$( 2)), summary$( 2%)        , ch(39),~
               at (07,02), fac(sfac$( 3)), summary$( 3%)        , ch(39),~
               at (08,02), fac(sfac$( 4)), summary$( 4%)        , ch(39),~
               at (09,02), fac(sfac$( 5)), summary$( 5%)        , ch(39),~
               at (10,02), fac(sfac$( 6)), summary$( 6%)        , ch(39),~
               at (11,02), fac(sfac$( 7)), summary$( 7%)        , ch(39),~
               at (12,02), fac(sfac$( 8)), summary$( 8%)        , ch(39),~
               at (13,02), fac(sfac$( 9)), summary$( 9%)        , ch(39),~
               at (14,02), fac(sfac$(10)), summary$(10%)        , ch(39),~
               at (15,02), fac(sfac$(11)), summary$(11%)        , ch(39),~
               at (16,02), fac(sfac$(12)), summary$(12%)        , ch(39),~
               at (17,02), fac(sfac$(13)), summary$(13%)        , ch(39),~
               at (18,02), fac(sfac$(14)), summary$(14%)        , ch(39),~
               at (19,02), fac(sfac$(15)), summary$(15%)        , ch(39),~
                                                                         ~
               at (05,42), fac(sfac$(16)), summary$(16%)        , ch(39),~
               at (06,42), fac(sfac$(17)), summary$(17%)        , ch(39),~
               at (07,42), fac(sfac$(18)), summary$(18%)        , ch(39),~
               at (08,42), fac(sfac$(19)), summary$(19%)        , ch(39),~
               at (09,42), fac(sfac$(20)), summary$(20%)        , ch(39),~
               at (10,42), fac(sfac$(21)), summary$(21%)        , ch(39),~
               at (11,42), fac(sfac$(22)), summary$(22%)        , ch(39),~
               at (12,42), fac(sfac$(23)), summary$(23%)        , ch(39),~
               at (13,42), fac(sfac$(24)), summary$(24%)        , ch(39),~
               at (14,42), fac(sfac$(25)), summary$(25%)        , ch(39),~
               at (15,42), fac(sfac$(26)), summary$(26%)        , ch(39),~
               at (16,42), fac(sfac$(27)), summary$(27%)        , ch(39),~
               at (17,42), fac(sfac$(28)), summary$(28%)        , ch(39),~
               at (18,42), fac(sfac$(29)), summary$(29%)        , ch(39),~
               at (19,42), fac(sfac$(30)), summary$(30%)        , ch(39),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L44425
                     call "MANUAL" ("CRCINPUT")
                     goto L44125

L44425:        if keyhit% <> 15% then L44445
                     call "PRNTSCRN"
                     goto L44125

L44445:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5                          /* Display Mode Only          */
           pf$(1) = "(1)Start Application Over (Don't Accept)          "&~
                    "             (13)Instructions"
           pf$(2) = "(2)First  (4)Prev                                 "&~
                    "             (15)Print Screen"
           pf$(3) = "(3)Last   (5)Next                           (14)Tr"&~
                    "ial Balance  (16)Accept Distr"
           pfkey$ = hex(0102030405ffffffffffffff0d0e0f10ffffff00)
           if l1% <> 0% then L44520
                str(pf$(2),,20) = " "
                str(pfkey$,2,1), str(pfkey$,4,1) = hex(ff)
L44520:    if l1% + 30% < auto_used% then L44535
                str(pf$(3),,20) = " "
                str(pfkey$,3,1), str(pfkey$,5,1) = hex(ff)
L44535:    return


        REM *************************************************************~
            *          C H E C K - O F F    S C R E E N                 *~
            * --------------------------------------------------------- *~
            * Check-Off Application Screen.                             *~
            *************************************************************

        deffn'106
            gosub set_pf6
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            auto_hdrs$(1) = "Settlement"
            auto_hdrs$(2) = "Srce Doc"
            auto_hdrs$(3) = "    Balance"
            auto_hdrs$(4) = "T/C"
            inpmessage$  = "Enter a non-blank character next to paid" &  ~
                           " items."
            init(hex(8c)) sfac$(), lfac$()
            init(" ") summary$()
            for dl% = 1% to 30%
              i% =  dl% + l1% : if i% > auto_used% then L45136
                lfac$(dl%) = hex(81)
                convert str(auto_idx$(i%),9,4) to c%
                str(summary$(dl%), 1) = str(stlmnt$(c%),1,8) & " " &     ~
                                        str(stlmnt$(c%),9,2)
                str(summary$(dl%),13) = srcedoc$(c%)
                convert prev(c%) to str(summary$(dl%),22),               ~
                                                        pic(-#######.00)
                if str(auto_idx$(i%),,1) <> " " then sfac$(dl%) = hex(84)
                str(summary$(dl%),33,5) = hex(8c) & currency$(c%)
                if currency$(c%) =  currency$ then                       ~
                                 str(summary$(dl%),33,1) = hex(84)
L45136:     next dl%

L45145:     accept                                                       ~
               at (01,02),                                               ~
                     "Cash Receipts Entry- Check-Off Application  ",     ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,04), fac(hex(ac)), auto_hdrs$(1)          , ch(11),~
               at (04,16), fac(hex(ac)), auto_hdrs$(2)          , ch( 8),~
               at (04,25), fac(hex(ac)), auto_hdrs$(3)          , ch(11),~
               at (04,37), fac(hex(ac)), auto_hdrs$(4)          , ch( 4),~
               at (04,44), fac(hex(ac)), auto_hdrs$(1)          , ch(11),~
               at (04,56), fac(hex(ac)), auto_hdrs$(2)          , ch( 8),~
               at (04,65), fac(hex(ac)), auto_hdrs$(3)          , ch(11),~
               at (04,77), fac(hex(ac)), auto_hdrs$(4)          , ch( 4),~
                                                                         ~
               at (05,02), fac(lfac$( 1)), str(auto_idx$(l1% +  1%),,1), ~
               at (06,02), fac(lfac$( 2)), str(auto_idx$(l1% +  2%),,1), ~
               at (07,02), fac(lfac$( 3)), str(auto_idx$(l1% +  3%),,1), ~
               at (08,02), fac(lfac$( 4)), str(auto_idx$(l1% +  4%),,1), ~
               at (09,02), fac(lfac$( 5)), str(auto_idx$(l1% +  5%),,1), ~
               at (10,02), fac(lfac$( 6)), str(auto_idx$(l1% +  6%),,1), ~
               at (11,02), fac(lfac$( 7)), str(auto_idx$(l1% +  7%),,1), ~
               at (12,02), fac(lfac$( 8)), str(auto_idx$(l1% +  8%),,1), ~
               at (13,02), fac(lfac$( 9)), str(auto_idx$(l1% +  9%),,1), ~
               at (14,02), fac(lfac$(10)), str(auto_idx$(l1% + 10%),,1), ~
               at (15,02), fac(lfac$(11)), str(auto_idx$(l1% + 11%),,1), ~
               at (16,02), fac(lfac$(12)), str(auto_idx$(l1% + 12%),,1), ~
               at (17,02), fac(lfac$(13)), str(auto_idx$(l1% + 13%),,1), ~
               at (18,02), fac(lfac$(14)), str(auto_idx$(l1% + 14%),,1), ~
               at (19,02), fac(lfac$(15)), str(auto_idx$(l1% + 15%),,1), ~
                                                                         ~
               at (05,04), fac(sfac$( 1)), summary$( 1%)        , ch(37),~
               at (06,04), fac(sfac$( 2)), summary$( 2%)        , ch(37),~
               at (07,04), fac(sfac$( 3)), summary$( 3%)        , ch(37),~
               at (08,04), fac(sfac$( 4)), summary$( 4%)        , ch(37),~
               at (09,04), fac(sfac$( 5)), summary$( 5%)        , ch(37),~
               at (10,04), fac(sfac$( 6)), summary$( 6%)        , ch(37),~
               at (11,04), fac(sfac$( 7)), summary$( 7%)        , ch(37),~
               at (12,04), fac(sfac$( 8)), summary$( 8%)        , ch(37),~
               at (13,04), fac(sfac$( 9)), summary$( 9%)        , ch(37),~
               at (14,04), fac(sfac$(10)), summary$(10%)        , ch(37),~
               at (15,04), fac(sfac$(11)), summary$(11%)        , ch(37),~
               at (16,04), fac(sfac$(12)), summary$(12%)        , ch(37),~
               at (17,04), fac(sfac$(13)), summary$(13%)        , ch(37),~
               at (18,04), fac(sfac$(14)), summary$(14%)        , ch(37),~
               at (19,04), fac(sfac$(15)), summary$(15%)        , ch(37),~
                                                                         ~
               at (05,42), fac(lfac$(16)), str(auto_idx$(l1% + 16%),,1), ~
               at (06,42), fac(lfac$(17)), str(auto_idx$(l1% + 17%),,1), ~
               at (07,42), fac(lfac$(18)), str(auto_idx$(l1% + 18%),,1), ~
               at (08,42), fac(lfac$(19)), str(auto_idx$(l1% + 19%),,1), ~
               at (09,42), fac(lfac$(20)), str(auto_idx$(l1% + 20%),,1), ~
               at (10,42), fac(lfac$(21)), str(auto_idx$(l1% + 21%),,1), ~
               at (11,42), fac(lfac$(22)), str(auto_idx$(l1% + 22%),,1), ~
               at (12,42), fac(lfac$(23)), str(auto_idx$(l1% + 23%),,1), ~
               at (13,42), fac(lfac$(24)), str(auto_idx$(l1% + 24%),,1), ~
               at (14,42), fac(lfac$(25)), str(auto_idx$(l1% + 25%),,1), ~
               at (15,42), fac(lfac$(26)), str(auto_idx$(l1% + 26%),,1), ~
               at (16,42), fac(lfac$(27)), str(auto_idx$(l1% + 27%),,1), ~
               at (17,42), fac(lfac$(28)), str(auto_idx$(l1% + 28%),,1), ~
               at (18,42), fac(lfac$(29)), str(auto_idx$(l1% + 29%),,1), ~
               at (19,42), fac(lfac$(30)), str(auto_idx$(l1% + 30%),,1), ~
                                                                         ~
               at (05,44), fac(sfac$(16)), summary$(16%)        , ch(37),~
               at (06,44), fac(sfac$(17)), summary$(17%)        , ch(37),~
               at (07,44), fac(sfac$(18)), summary$(18%)        , ch(37),~
               at (08,44), fac(sfac$(19)), summary$(19%)        , ch(37),~
               at (09,44), fac(sfac$(20)), summary$(20%)        , ch(37),~
               at (10,44), fac(sfac$(21)), summary$(21%)        , ch(37),~
               at (11,44), fac(sfac$(22)), summary$(22%)        , ch(37),~
               at (12,44), fac(sfac$(23)), summary$(23%)        , ch(37),~
               at (13,44), fac(sfac$(24)), summary$(24%)        , ch(37),~
               at (14,44), fac(sfac$(25)), summary$(25%)        , ch(37),~
               at (15,44), fac(sfac$(26)), summary$(26%)        , ch(37),~
               at (16,44), fac(sfac$(27)), summary$(27%)        , ch(37),~
               at (17,44), fac(sfac$(28)), summary$(28%)        , ch(37),~
               at (18,44), fac(sfac$(29)), summary$(29%)        , ch(37),~
               at (19,44), fac(sfac$(30)), summary$(30%)        , ch(37),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L45605
                     call "MANUAL" ("CRCINPUT")
                     goto L45145

L45605:        if keyhit% <> 15% then L45625
                     call "PRNTSCRN"
                     goto L45145

L45625:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf6
           pf$(1) = "(1)Start Check Over  (6)Set All to Paid     (10)Ex"&~
                    "it to Header (13)Instructions"
           pf$(2) = "(2)First  (4)Prev    (7)Reset to Blanks     (11)Ex"&~
                    "it to Lines  (15)Print Screen"
           pf$(3) = "(3)Last   (5)Next    (8)Display Amounts     (14)Tr"&~
                    "ial Balance  (16)Accept Distr"
           pfkey$ = hex(0102030405060708ff0a0bff0d0e0f10ffffff00)
           if l1% <> 0% then L45700
                str(pf$(2),,20) = " "
                str(pfkey$,2,1), str(pfkey$,4,1) = hex(ff)
L45700:    if l1% + 30% < auto_used% then L45715
                str(pf$(3),,20) = " "
                str(pfkey$,3,1), str(pfkey$,5,1) = hex(ff)
L45715:    return


        REM *************************************************************~
            * C H E C K   O F F   A P P L I C A T I O N   S C R E E N   *~
            *-----------------------------------------------------------*~
            * Allows change of parameters for load                      *~
            *************************************************************

        deffn'107(fieldnr%, edit%)  /* 2% = Edit, 3% = Display         */
            str(line2$,,44) = "Cust: " & billto$ &                       ~
                              " Check: " & chknr$ & " Curr: " & currency$
            gosub set_pf7
            init(hex(86)) lfac$()
            if fieldnr% > 0% then init(hex(8c)) lfac$()
            on fieldnr% gosub      ,               /* Amounts          */~
                              L46250,               /* As Of Date       */~
                              L46250,               /* Stlmnt# Range    */~
                              L46250,               /* Date Type/Range  */~
                              L46250,               /* G/L Range        */~
                                   ,               /* Credits First?   */~
                                   ,               /* Force Balance?   */~
                              L46250                /* Currency?        */
            goto L46320

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L46250:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L46320:     accept                                                       ~
               at (01,02), "Cash Receipts Entry- Check Off Parameters",  ~
               at (01,50), "Post: ",                                     ~
               at (01,56), fac(hex(8c)), postdate$              , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of Date",                                 ~
               at (06,30), fac(lfac$( 2)), auto_asof$           , ch(08),~
                                                                         ~
               at (07,02), "Settlement # Range:",                        ~
               at (07,30), fac(lfac$( 3)), auto_stlmnt$(1)      , ch(08),~
               at (07,43), "to",                                         ~
               at (07,46), fac(lfac$( 3)), auto_stlmnt$(2)      , ch(08),~
                                                                         ~
               at (08,02), "Date Type (P/D)/Range:",                     ~
               at (08,27), fac(lfac$( 4)), auto_ranges$(1)      , ch(01),~
               at (08,30), fac(lfac$( 4)), auto_ranges$(2)      , ch(08),~
               at (08,43), "to",                                         ~
               at (08,46), fac(lfac$( 4)), auto_ranges$(3)      , ch(08),~
                                                                         ~
               at (09,02), "Include/Exclude G/L Accts:",                 ~
               at (09,30), fac(lfac$( 5)), auto_acct$(1)        , ch(12),~
               at (09,43), "to",                                         ~
               at (09,46), fac(lfac$( 5)), auto_acct$(2)        , ch(12),~
               at (09,60), "Include/Exclude",                            ~
               at (09,76), fac(lfac$( 5)), auto_acctie$         , ch(01),~
                                                                         ~
               at (10,02), "Check Currency Only?",                       ~
               at (10,30), fac(lfac$( 8)), auto_currchk$        , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 13 then L46730  :  call "MANUAL" ("CRCINPUT")
                                            goto L46320
L46730:     if keyhit% <> 15 then L46750  :  call "PRNTSCRN"
                                            goto L46320
L46750:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return


        set_pf7

         if fieldnr% > 0% then L46865     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                                     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Check Header                          (14)Trial"&~
                    " Balance     (16)Load Stlmnts"
           pfkey$ = hex(0102ffffffffffff09ffffff0d0e0f10ffffff00)
           return

                                         /* Edit Mode- Field Enabled   */
L46865:    pf$(1) = "(1)Start Check Over                               "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* Bill-to Number   */~
                                    L50480,         /* Check Number     */~
                                    L50660,         /* Check Date       */~
                                    L50700,         /* Bank Number      */~
                                    L50730,         /* Post Mark Date   */~
                                    L50810,         /* Net Check Amount */~
                                    L51030          /* Currency code    */
                  return

L50160
*        Bill-to Number                        BILLTO$
            billto_name$ = hex(06) & "Select Bill-to"
            call "GETCODE" (#03, billto$, billto_name$, 0%, 1.3, f1%(3%))
            if f1%(3%) = 1% then L50194
                errormsg$ = "Bill-to not on file."  :  return
L50194:     get #03 using L50196, billxref$, currency$
L50196:         FMT POS(780), CH(9), POS(1045), CH(4)
            billto_ar = 0                                       /* JIC */
            call "READ100" (#51, billto$, f1%(51%))        /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L50206, billto_ar
L50206:         FMT POS(130), PD(14,4)
            if curr$ = "Y" then goto L50223
                currency$, currdesc$ = " " : goto L50230
L50223:     call "DESCRIBE" (#40, currency$, currdesc$, 0%, f1%(40))
L50230:     if billxref$ = billto$ then L50300
                plowkey$ = str(billto$) & hex(00)
                call "PLOWNEXT" (#06, plowkey$, 9%, f1%(6))
                if f1%(6) = 1% then L50300
                     errormsg$ = "Customer is not a Bill-to Customer."
                     return
L50290:
L50300:  /* Task out other users from posting to this bill-to          */
            readkey$ = str(billto$) & hex(0000000000000000)
            write #09 using L50350, readkey$, userid$, " ",               ~
                                  hex(00), userid$, readkey$, " ",       ~
                                  eod goto L50380
L50350:         FMT CH(17), CH(3), CH(180), CH(1), CH(5), CH(17), CH(77)
            tasking% = 1%
            goto L50440
L50380:         call "READ100" (#09, readkey$, f1%(9))
                if f1%(9) = 0% then L50290
                     errormsg$ = "Bill-to is already being posted to by "~
                                 & str(key(#09,1%),2,3)
                     return

L50440:     temp = billto_ar
            call "CONVERT" (temp, 2.2, billto_ar$)
            return


L50480
*        Check Number                          CHKNR$
            if chknr$ <> " " then L50560
                plowkey$ = hex(06) & "Following Checks are in Buffer"
                readkey$ = str(session$) &  billto$
                call "PLOWCODE" (#09, readkey$, plowkey$, 15%, 1, f1%(9))
                if f1%(9) = 1% then L50550
                     errormsg$ = hex(00)  :  return
L50550:         chknr$ = str(readkey$,16)
L50560:     readkey$ = str(billto$) & chknr$
            call "READ100" (#05, readkey$, f1%(5))
            if f1%(5) = 0% then L50610
                errormsg$ = "Check already exists for Bill-to"
                return
L50610:     gosub load_check
                if checkonfile% = 0% or errormsg$ <> " " then return
                     return clear all
                     goto check_header

L50660
*        Check Date                            CHKDATE$
            call "DATEOK" (chkdate$, u3%, errormsg$)
              if errormsg$ <> " " then return
            convert (999999% - u3%) to rev_date$, pic(000000)
            return

L50700
*        Bank Number                           BANKNR$
            return

L50730
*        Post Mark Date                        POSTMARK$
            call "DATEOK" (postmark$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (postmark$)
                postmarku$ = postmark$
                call "DATEFMT" (postmark$)
                return

L50810
*        Net Check Amount                      NETCHK$
            if netchk$ = " " then netchk$ = "0"
            convert netchk$ to netchk, data goto L50840  :  goto L50860
L50840:         errormsg$ = "Invalid Entry for Net Check Amount"
                return
L50860:     if netchk >= -999999.99 and netchk <= 9999999.99 then L51000
                errormsg$ = "Entry for Net Check Amount is too large" &  ~
                            " (-999999.99 to 9999999.99)"
                return
L51000:     call "CONVERT" (netchk, 2.2, netchk$)
            return

L51030
*        Currency code                       CURRENCY$
            convdate$ = " " : conveqv, convunt = 1
            if curr$ <> "Y" then return
            if currency$ = " " then currency$ = statutory$
            if currency$ = "?" then currency$ = " "
            call "GETCODE" (#40, currency$, currdesc$, 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto L51110
L51090:         errormsg$ = "Invalid Currency code.  Try again." : return
L51110:     if currency$ = statutory$ then return
            currkey$ = str(currtype$) & str(currency$) & rev_date$
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
               if f1%(41) = 0% then L51090
            get #41 using L51160, convdate$, conveqv, convunt
L51160:         FMT POS(12), CH(6), 2*PD(14,7)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests Data for Line Item Entries.                         *~
            *************************************************************

            deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52080,               /* Posting Type     */~
                              L52120,               /* Settlement Nr    */~
                              L52300,               /* PO               */~
                              L52315,               /* Due Date         */~
                              L52355,               /* Credit Acct      */~
                              L52400                /* Amounts          */
            return

L52080
*        Posting Type
            if pos("PABUGS" = type$(c%)) > 0% then L52100
                errormsg$ = "Invalid Posting Type."
                return
L52100:     gosub descr_type
            if pos("PA" = type$(c%)) > 0% then least% = 6%
            return

L52120
*        Settlement Number  (Types P and A Only)
          gosub check_bankrupt                     /* (EWD001) */
          if bankrupt% = 1% then                                ~
             call "SHOSTAT" ("Settlement Number Part of BankRuptcy!!")
          if bankrupt% = 1% then stop
          if pos("PA" = type$(c%)) = 0% then return
            if stlmnt$(c%) <> " " then L52145
                errormsg$ = "Settlement Number may not be blank."
                return
L52145:     if str(stlmnt$(c%),9) = " " then str(stlmnt$(c%),9,2) = "00"
            for x% = 1% to maxlines%
                if x% = c% then L52180
                if stlmnt$(c%) <> stlmnt$(x%) then L52180
                     errormsg$ = "Settlement already applied to on" &    ~
                                 " this check."
                     return
L52180:     next x%
            readkey$ = str(billto$) & str(stlmnt$(c%),,10) & "00"
            call "READ100" (#06, readkey$, f1%(6))
            if f1%(6) = 1% then L52205
                errormsg$ = "Settlement Number not on file." : return
L52205:     get #06 using L52220, discpct(c%), duedate$(c%), grace%,      ~
                                po$(c%), cracct$(c%), srcetype$(c%),     ~
                                srcedoc$(c%)
L52220:         FMT POS(22), PD(14,4), CH(6), BI(1), XX(6), CH(16),      ~
                    POS(76), CH(9), XX(2), CH(2), CH(8)
            call "DATE" addr("G+", duedate$(c%),  grace%, adjdue$(c%),   ~
                                                                     u3%)
            call "GLFMT" (cracct$(c%))
            auto_asofu$ = date
            gosub call_armbalnc
            prev (c%) = bals(3)
            other(c%) = bals(2)
            net  (c%) = bals(3)
            if adjdue$(c%) < postmarku$ then L52285
                disca(c%) = round(net(c%) * discpct(c%) * (-.01), 2)
                net  (c%) = net(c%) + disca(c%)
L52285:     gosub describe_line
            return

L52300
*        PO
            return

L52315
*        Due Date  (BUGS only)
          if pos("PA" = type$(c%)) <> 0% then return
            call "DATEOK" (adjduef$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (adjduef$)
                adjdue$(c%), duedate$(c%) = adjduef$
                gosub descr_due
                return

L52355
*        Credit Account  (BUGS only)
            cracctdescr$ = hex(06) & "Select Credit Account"
            call "GETCODE" (#04, cracct$(c%), cracctdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then L52385
                errormsg$ = "Invalid Credit Account"
                return
L52385:     gosub descr_acct
            return

L52400
*        Amounts
            gross, disca, discu, net = 0
            if pos("GS" = type$(c%)) <> 0% then gross$,disca$,discu$=" "
            if gross$ <> " " then convert gross$ to gross,data goto L52435
            if disca$ <> " " then convert disca$ to disca,data goto L52440
            if discu$ <> " " then convert discu$ to discu,data goto L52445
            if net$   <> " " then convert net$   to net  ,data goto L52450
            goto L52455
L52435:        errormsg$ = "Invalid entry for Gross Payment."   : return
L52440:        errormsg$ = "Invalid entry for Discount Amount." : return
L52445:        errormsg$ = "Invalid entry for Unallowed Disc."  : return
L52450:        errormsg$ = "Invalid entry for Net Payment Amt." : return
L52455:     if gross$ = " " then gross = net  - (disca + discu)
            if net$   = " " then net   = gross + disca + discu
            if disca$ = " " then disca = net   - discu - gross
            if discu$ = " " then discu = net   - disca - gross
            net = gross + disca + discu
            disca(c%) = disca
            discu(c%) = discu
            net  (c%) = net
            gosub descr_amts
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the Auto Application Parameters             *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53200,         /* Amounts          */~
                                    L53400,         /* As Of Date       */~
                                    L53450,         /* Settlement Range */~
                                    L53500,         /* Dates Type/Range */~
                                    L53700,         /* G/L Range        */~
                                    L53850,         /* Credits First?   */~
                                    L53900,         /* Force Balance?   */~
                                    L53950          /* Check Currency?  */
                  return

L53200
*        Amounts
            auto_amts(1,1), auto_amts(1,2), auto_amts(1,3) = 0
            if auto_amts$(1) <> "CALC" or auto_amts$(2) <> "CALC"        ~
                                                               then L53250
          /* Case 1: Gross, Disc = Calc --> Net must be entered */
            if auto_amts$(3) = " " then auto_amts$(3) = "0"
            convert auto_amts$(3) to auto_amts(1,3), data goto L53370
            call "CONVERT" (auto_amts(1,3), 2.2, auto_amts$(3))
            return

L53250:   /* Case 2: Two or three values entered.               */
            if auto_amts$(1) = "CALC" then auto_amts$(1) = " "
            if auto_amts$(2) = "CALC" then auto_amts$(2) = " "
            if auto_amts$(1) <> " " then                                 ~
                convert auto_amts$(1) to auto_amts(1,1), data goto L53360
            if auto_amts$(2) <> " " then                                 ~
                convert auto_amts$(2) to auto_amts(1,2), data goto L53365
            if auto_amts$(3) <> " " then                                 ~
                convert auto_amts$(3) to auto_amts(1,3), data goto L53370

            if auto_amts$(1) = " "  then                                 ~
                        auto_amts(1,1) = auto_amts(1,3) - auto_amts(1,2)
            if auto_amts$(3) = " "  then                                 ~
                        auto_amts(1,3) = auto_amts(1,1) + auto_amts(1,2)
            if auto_amts$(2) = " "  then                                 ~
                        auto_amts(1,2) = auto_amts(1,3) - auto_amts(1,1)
            auto_amts(1,3) = auto_amts(1,1) + auto_amts(1,2)

            call "CONVERT" (auto_amts(1,1), 2.2, auto_amts$(1))
            call "CONVERT" (auto_amts(1,2), 2.2, auto_amts$(2))
            call "CONVERT" (auto_amts(1,3), 2.2, auto_amts$(3))
            return
L53360:         errormsg$ = "Invalid Entry for Gross Amount"  :  return
L53365:         errormsg$ = "Invalid Entry for Discount Amt"  :  return
L53370:         errormsg$ = "Invalid Entry for Net Amount"    :  return

L53400
*        As Of Date
            call "DATEOK" (auto_asof$, u3%, errormsg$)
            return

L53450
*        Settlement Range
          call "TESTRNGE" (auto_stlmnt$(1), auto_stlmnt$(2),             ~
                           auto_stlmnt$(3), auto_stlmnt$(4), errormsg$)
          return

L53500
*        Range Selection
            if auto_ranges$(1) = " " then L53645
            if pos("PD" = auto_ranges$(1)) > 0% then L53525
                errormsg$ = "Enter 'P', 'D', or blank."
                return
L53525
*        Range Specification
            if auto_ranges$(2) = "ALL" then L53645
                if auto_ranges$(2) = "FIRST" then L53565
                     call "DATEOK" (auto_ranges$(2), u3%, errormsg$)
                        if errormsg$ <> " " then return
                     auto_ranges$(4) = auto_ranges$(2)
                     call "DATUNFMT" (auto_ranges$(4))
                        goto L53575
L53565:            auto_ranges$(4) = all(hex(00))

L53575:         if auto_ranges$(3) = "LAST" then L53620
                if auto_ranges$(3) <> " " then L53595
                   if auto_ranges$(4) = hex(0000000000000000) then L53630
                      auto_ranges$(3) = auto_ranges$(2)
L53595:            call "DATEOK" (auto_ranges$(3), u3%, errormsg$)
                      if errormsg$ <> " " then return
                   auto_ranges$(5) = auto_ranges$(3)
                   call "DATUNFMT" (auto_ranges$(5))
                   goto L53625
L53620:     auto_ranges$(5) = all(hex(ff))
L53625:     if auto_ranges$(4) <= auto_ranges$(5) then return
L53630:        errormsg$ = "Invalid Date Range Selection"
               return

L53645:     auto_ranges$(2) = "ALL"
            auto_ranges$(4) = all(hex(00))
            auto_ranges$(3) = " "
            auto_ranges$(5) = all(hex(ff))
            return

L53700
*        Select G/L Account
            if auto_acct$(1) = "ALL" then L53795
            if (pos("IE" = auto_acctie$) = 0%) then auto_acctie$ = "I"
                call "GLVALID" (auto_acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#4, auto_acct$(1), temp$, 0%, 0, f1%(4))
                if auto_acct$(2) = " " then auto_acct$(2) = auto_acct$(1)
                call "GLVALID" (auto_acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#4, auto_acct$(2), temp$, 0%, 0, f1%(4))
                if auto_acct$(2) < auto_acct$(1) then                    ~
                                            errormsg$ = "Invalid Range"
                auto_acct$(3) = auto_acct$(1)
                call "GLUNFMT" (auto_acct$(3))
                auto_acct$(4) = auto_acct$(2)
                call "GLUNFMT" (auto_acct$(4))
                return
L53795:     auto_acct$(2) = " "
            auto_acct$(3) = "ALL"
            auto_acct$(4) = " "
            auto_acctie$ = "I"
            return

L53850
*        Process Credits First?
            if pos("YN" = credits_first$) > 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L53900
*        Force Balancing?
            if pos("YN" = force_balance$) > 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L53950
*        Check Currency Only?
            if pos("YN" = auto_currchk$) > 0% then return
                errormsg$ = "ENTER 'Y' OR 'N'."
                return

/*  (EWD001)  */
        check_bankrupt
           bankrupt% = 0%
           init(" ") readkey$
           str(readgen$,1%,9%) = "BANKRUPT"
           str(readgen$,10%,15%) = str(stlmnt$(c%),1%,8%)
           read #11, key = readgen$, eod goto L55210
           bankrupt% = 1%
L55210: return

/*  (EWD001)  - End */
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
            u3% = 2%
            call "UPDUSRLG" ("CRCUPDTE", " ", " ", " ", session$, u3%,   ~
                " ", " ")
            end