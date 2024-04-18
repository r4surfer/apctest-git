        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCSAUD                             *~
            *  Creation Date     - 02/26/96                             *~
            *  Last Modified Date- 11/20/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Verifys all A/R         *~
            *                      Information for a Customer against   *~
            *                      the Live Data in A/R Invoice, Cash,  *~
            *                      and Sales.                           *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/02/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/20/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            *************************************************************

        dim                                                              ~
            scan_cust$9,                 /* CUSTOMER SCAN KEY          */~
            shipto_cust$9,               /* SHIP TO CUSTOMER           */~
            ver_msg$50,                  /* REPORT MESSAGE             */~
            settlement$32,               /* READ VERBUFFR RECORD       */~
            ver_msg$(5%)50,              /* STANDARD ERROR MSGS        */~
            company$60,                  /* Company name               */~
            rpttitle$60,                 /* Report Title               */~
            time$8,                      /* Run Time for Report        */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            recnum%(6%),                 /*  NUMBER OF RECORDS ARRAY   */~
            sysvol$6,                    /* SYSTEM VOLUME & LIBRARY    */~
            updtemsg$80,                 /* SHOSTAT MSG STRING         */~
            userid$3                     /* Current User Id            */

        dim                                                              ~
           inv_key$17,                   /* 'ARIMASTR' SCAN KEY        */~
           srce_err$1,                   /* ERROR FLAG                 */~
           srce_ver1$1,                  /* CHECK FLAG                 */~
           srce_ver2$1,                  /* TRIAL FLAG                 */~
           srce_ver3$1,                  /* INVOICE FLAG               */~
           srce_key$21,                  /* KEY FOR LOOKING UP CHECK   */~
           scankey$21,                   /* KEY FOR 'CRCLINES' LOOKUP  */~
           source_key$8,                 /* CHECK OR VOUCHER NO.       */~
           curr_cust$9,                  /* SAVE CURRENT CUSTOMER      */~
           beg_cust$9,                   /* Archive Customer or 'ALL'  */~
           custname$30,                  /* Customer Name              */~
           settle_1$12,                  /* SETTLEMENT                 */~
           settle_2$12,                  /* SETTLEMENT                 */~
           settle_key$12,                /* TRIAL SETTLEMENT KEY       */~
           type$1,                       /* TRANSACTION TYPE           */~
           srce_type$1,                  /* CHECK SRCE CURR TXN TYPE   */~
           trial_key$21,                 /* TRIAL SCAN KEY             */~
           trial_typ1$1,                 /* TXN TYPE - SOURCE          */~
           trial_typ2$1                  /* TXN TYPE - APPLIED AGAINST */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /*                            */~
            axd$4,                       /* AXD Block for OPENFILE     */~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Audit Utility to Verify A/R       "
            pname$ = "APCCSAUD - Rev: R6.04"

        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! A/R Trial Balance            (Production)*~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! CUSTOMER ! Customer Master File.                    *~
            * #12 ! ARIMASTR ! Invoice Master File          (Production)*~
            * #13 ! ARILINES ! Invoice Lines File           (Production)*~
            * #14 ! CRCMASTR ! Cash Receipts Master File    (Production)*~
            * #15 ! CRCLINES ! Cash Receipts Lines  File    (Production)*~
            * #20 ! VERBUFFR ! AR Verify work area          (Production)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21                      ~

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #12, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen = 17,                      ~
                        alt key  1, keypos =  10, keylen =  8 , dup,     ~
                            key  2, keypos =  18, keylen =  16, dup,     ~
                            key  3, keypos =  34, keylen =  16, dup,     ~
                            key  4, keypos =1783, keylen =  26

            select #13, "ARILINES",                                      ~
                        varc,     indexed,  recsize = 750,               ~
                        keypos =    1, keylen =  20

            select #14, "CRCMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen = 17

            select #15, "CRCLINES",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =  21

            select #20, "VERBUFFR",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =    1, keylen = 20,                      ~
                        alt key  1, keypos =   9, keylen = 12, dup


         mat recnum% = zer
         sysvol$,syslib$ = all(hex(20))

        call "SHOSTAT" ("Opening Files, One Moment Please")

        /*  OPEN FILES IN USER'S DEFAULT DATABASE TO BE SCANNED  */

            call "OPENFILE" (#1, "SHARE", f2%(1%), rslt$(1%), axd$)

            call "OPENFILE" (#2, "SHARE", f2%(2%), rslt$(2%), axd$)

            call "OPENFILE" (#12, "SHARE",f2%(12%), rslt$(12%),axd$)

            call "OPENFILE" (#13, "SHARE",f2%(13%), rslt$(13%),axd$)

            call "OPENFILE" (#14, "SHARE",f2%(14%), rslt$(14%),axd$)

            call "OPENFILE" (#15, "SHARE",f2%(15%), rslt$(15%),axd$)

        REM - OPEN 'VERBUFFR' File
            arc_buf% = 0%

            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 4000%, rslt$(20%))
            arc_buf% = 1%
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date  : call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "COMPNAME" (0%, company$,0%)
            rpttitle$ = "A/R Customer Transaction Error Report"
            select printer(134)
            time$ = " " : call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99%

            updtemsg$ = "Acct(XXXXXXXXX) ERR=(XXXX)"

            ver_msg$(1) = "Check Not Found in 'CRCMASTR' File.     "
            ver_msg$(2) = "Settlement Not Found in 'ARMTRIAL' File."
            ver_msg$(3) = "Invoice Not Found in 'ARIMASTR' File.   "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, beg_cust$, custname$

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10170
L10130:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10130
L10170:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Verify   "
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       verify_routine
                  if keyhit% <>  0 then       editpg1
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf16$ = " "
L11180:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
            goto editpg1


        REM *************************************************************~
            *              V E R I F Y   R O U T I N E                  *~
            * --------------------------------------------------------- *~
            * Notes- (1) Driving file is the 'CRCLINES' File.           *~
            *        (2) Load all Check References into the buffer file.*~
            *            and check for settlements in the 'ARNTRIAL' File~
            *        (3) Verify the trial file against the buffer file  *~
            *            and insure all payments found. Add invoices    *~
            *            with no payments or just adjustments (Inv).    *~
            *        (4) Verify 'ARIMASTR' File against the buffer file *~
            *            and insure all invoices found.                 *~
            *        (5) Print all errors in the verification log.      *~
            *        (6) Process another customer.                      *~
            *                                                           *~
            *                                                           *~
            *************************************************************
        verify_routine

*       ****************************************************************~
*        Main read loop to read all records in 'CRCLINES' and Build    *~
*        'VERBUFFR' File.                                              *~
*         Field (1) - Pos (1 thru 8) Soruce Document (Check No.)  (08) *~
*                                                                      *~
*         Field (2) - Pos (9 thru 20) Settlement No.              (12) *~
*                                                                      *~
*         Field (3) - Pos (21 thru 21) Error Flag                 (01) *~
*                                                                      *~
*         Field (4) - Pos (22 thru 22) Type of Settlement.        (01) *~
*                     (0) = Cash                                       *~
*                     (1) = Unapplied                                  *~
*                     (2) = Invoice                                    *~
*                                                                      *~
*         Field (5) - Pos (23 thru 23) Verified Ok (Y or N) CHECKS (01)*~
*         Field (6) - Pos (24 thru 24) Verified Ok (Y or N) TRIAL  (01)*~
*         Field (7) - Pos (25 thru 25) Verified Ok (Y or N) INVOICE(01)*~
*                                                                      *~
*         Primary Key -     = Fields (1) and (2)                       *~
*         Alternate Key (1) = Field  (2)                               *~
*                                                                      *~
*                                                                      *~
*       ****************************************************************
        REM MAIN_LOOP
          call "SHOSTAT" ("Verifying Customer Accounts")
          chk% = 0%
          scan_cust$, scankey$   = " "
          if beg_cust$ <> "ALL" then goto L12470
             scan_cust$, curr_cust$ = " "
             goto L12540
L12470:
          curr_cust$ = beg_cust$         /* SET FOR ACCOUNT VERIFY    */
          scan_cust$ = curr_cust$
          read #3,key = scan_cust$, eod goto end_routine
          goto L12580

        next_customer
L12540:                                  /*PULL CUSTOMERS FROM TRIAL  */
          read #3,key > scan_cust$, eod goto end_routine
          get #3 using L12570, scan_cust$
L12570:     FMT CH(9)
L12580:   curr_cust$ = scan_cust$

          str(scankey$,1,9) = curr_cust$
          str(scankey$,10,12) = " "
        REM   CALL "SHOSTAT" ("Verifying Acct ("&CURR_CUST$&")")

          main_loop_next

            source_key$, type$, settle_1$, settle_2$, settle_key$ = " "
            srce_err$, srce_ver1$, srce_ver2$, srce_ver3$         = "Y"
            srce_type$  = "0"       /* '0'=Cash, '1'=Unapplied,        */
                                    /* '2'= Invoice                    */
            read #15,key > scankey$, eod goto L12750   /* SCAN CUSTOMER */
              get #15, using L12720, scankey$, type$, settle_1$,settle_2$
L12720:         FMT CH(21), CH(1), 2*CH(12)

              if curr_cust$ = str(scankey$,1,9) then goto L12820
L12750:          gosub complete_verify
                 if error% = 0% then goto L12800
                    str(updtemsg$,6,9) = curr_cust$
                    convert error% to str(updtemsg$,22,4), pic(####)
                    call "SHOSTAT" (updtemsg$)
L12800:          if beg_cust$ <> "ALL" then goto end_routine
                 goto next_customer
L12820:                                            /* SKIP 'G' ENTRIES */
              if type$ = "G" then goto main_loop_next
                 source_key$ = str(scankey$,10,8)
                 settle_key$ = str(settle_2$,1,8) & "0000"
                 if type$ <> "U" then goto L12890      /* CHECK     */
                    settle_key$ = str(settle_1$,1,8) & "0000"
                    srce_type$  = "1"                 /* UNAPPLIED */
L12890:
                 gosub verify_check   /* VERIFY SETTLEMENT IN TRIAL   */
                 gosub check_key_1    /* 1ST PUT CHECK AND SETTLEMENT */
                                      /* INTO BUFFER FILE.            */
        goto main_loop_next

        REM ***********************************************************
        REM ***********************************************************

        update_buffer_chg
          str(srce_key$,1,8) = source_key$
          str(srce_key$,9,12)= settle_key$
          call "DELETE" (#20, srce_key$, 20%)

        update_buffer_add
          write #20, using L13070, source_key$, settle_key$, srce_err$,   ~
                                  srce_type$, srce_ver1$, srce_ver2$,    ~
                                  srce_ver3$, " "
L13070:     FMT CH(8), CH(12), 5*CH(1), CH(7)
          chk% = chk% + 1%
        return

        check_key_1                      /* CHECK VERIFY BUFFER       */
          str(srce_key$,1,8)   = source_key$
          str(srce_key$,9,12)  = settle_key$
          read #20,key = srce_key$, eod goto update_buffer_add
        return

        verify_check                /* INSURE ALL SETTLEMENTS IN TRIAL */
          str(trial_key$,1,9)  = curr_cust$
          str(trial_key$,10,12)= settle_key$
          read #1,key = trial_key$, eod goto L13220
             return            /* SETTLEMENT FOR CHECK FOUND IN TRIAL */
L13220:                        /* ERROR - NOT FOUND IN ARMTRIAL       */
          srce_err$, srce_ver2$ = "N"
        return

        verify_settle
          str(inv_key$,1,9)  = shipto_cust$
          str(inv_key$,10,8) = str(settle_key$,1,8)
          read #12,key = inv_key$, eod goto L13310
             return
L13310:   srce_err$, srce_ver3$ = "N" /* ERROR - NOT FOUND IN ARIMASTR*/
        return

        REM *************************************************************~
            *        C H E C K   T R I A L   R E C O R D S              *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        verify_trial
          str(trial_key$,1,9)   = curr_cust$
          str(trial_key$,10,12) = " "
        next_trial

          read #1,key > trial_key$, eod goto L13840
          get #1 using L13480, trial_key$, trial_typ1$, trial_typ2$,      ~
                              source_key$, shipto_cust$
L13480:     FMT CH(21), POS(87), 2*CH(1), CH(8), POS(109), CH(9)

          if curr_cust$ <> str(trial_key$,1,9) then return
             srce_err$, srce_ver1$, srce_ver2$,srce_ver3$ = "Y"
             if str(trial_key$,10,8) <> source_key$ then goto L13730
                if trial_typ1$ <> "I" then goto next_trial /* UNAPPLIED*/
L13540:             srce_type$ = "2"         /* INV OR INV ADJ         */
                    str(settle_key$,1,8) = source_key$
                    str(settle_key$,9,4) = "0000"
                    if shipto_cust$ <> " " then goto L13590
                       shipto_cust$ = curr_cust$
L13590:                gosub verify_settle

                    call "REDALT0" (#20, settle_key$, 1%, f1%(20))
                    if f1%(20) = 1% then goto L13650
                       gosub update_buffer_add
                       goto next_trial
L13650:
                    if srce_err$ = "Y" then goto next_trial
                       get #20, using L13690, source_key$, settle_key$,   ~
                                       srce_type$, srce_ver1$, srce_ver2$
L13690:                  FMT CH(8), CH(12), XX(1),3*CH(1)
                       gosub update_buffer_chg
                       goto next_trial

L13730:
             if trial_typ1$ <> "I" then goto L13770  /* PAYMENT */
                goto L13540                   /* INV ADJUSTMENT */

L13770:                        /* ALL PAYMENT SHOULD BE IN BUFFER */
             str(settle_key$,1,8) = str(trial_key$,10,8)
             str(settle_key$,9,4) = "0000"
             srce_err$, srce_ver1$ = "N"     /* ERROR - IF NOT FOUND*/
             srce_type$ = "0"
             gosub check_key_1               /* AND ADDED          */
          goto next_trial
L13840:
        return

        REM *************************************************************~
            *       C H E C K   S E T T L E M E N T   R E C O R D S     *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        verify_settlement
          if chk% = 0% then return
          str(inv_key$,1,9) = curr_cust$
          str(inv_key$,10,8) = " "

        next_settlement
          read #12,key > inv_key$, eod goto L14150
            get #12 using L14010, inv_key$
L14010:       FMT CH(17)
            if curr_cust$ <> str(inv_key$,1,9) then return
               str(settle_key$,1,8) = str(inv_key$,10,8)
               str(settle_key$,9,4) = "0000"
               call "REDALT0" (#20, settle_key$, 1%, f1%(20))
               if f1%(20) = 0 then goto L14080
                  goto next_settlement
L14080:
               source_key$ = str(inv_key$,10,8)
               srce_err$, srce_ver3$ = "N"
               srce_ver1$,srce_ver2$ = "Y"
               srce_type$  = "2"
               gosub check_key_1           /* UPDATE ERROR */
          goto next_settlement
L14150: return

        complete_verify
          gosub verify_trial
          gosub verify_settlement
          gosub check_for_errors
          call "DELETE" (#20, " ", 0%)
          chk% = 0%
        return

        check_for_errors
          error% = 0%
          srce_key$ = " "
        next_record
          read #20,key > srce_key$, eod goto L14470
            get #20 using L14310, settlement$
L14310:       FMT CH(32)
          srce_key$ = str(settlement$,1,20)
          if str(settlement$,21,1) = "Y" then goto next_record
             ver_msg$ = " VERIFICATION ERROR FOUND. "
             for n% = 1% to 3%

             if str(settlement$,22+n%,1) <> "N" then goto L14450
                ver_msg$ = ver_msg$(n%)
                if lcntr% > 56% then gosub page_head
                print using L60280, curr_cust$, str(settlement$,1,8),     ~
                                   str(settlement$,9,12), ver_msg$
                print
                lcntr% = lcntr% + 2%
                error% = error% + 1%
L14450:      next n%
          goto next_record
L14470:
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub      L20130         /* CUSTOMER OR ALL  */

               return

L20130
*        Verify a Customer or 'ALL'                 BEG_CUST$
            inpmessage$ = "Enter 'ALL' or Specific Customer Id."  &      ~
                          " To Be Verified."
            return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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

        deffn'101(fieldnr%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub       L40150          /* Customer Account */


            goto L40180
                  lfac$(fieldnr%) = hex(80)  :  return   /* UpLow      */
L40150:           lfac$(fieldnr%) = hex(81)  :  return   /* UPPER Only */
                  lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

L40180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (07,02), "Audit a Customer or 'ALL'",                  ~
               at (07,31), fac(lfac$(1%)), beg_cust$            , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104050f10)), key(keyhit%)

               if keyhit% <> 15% then L40380
                  call "PRNTSCRN"
                  goto L40180

L40380:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr%  gosub      L50120          /* Starting Cust */

               return

L50120: REM - Audit Customer to Start
           errormsg$ = " "
           if beg_cust$ = "ALL" then return
           if beg_cust$ <> " " then goto L50180
              goto L50210

L50180:    call "READ100" (#3, beg_cust$, f1%(3))
           if f1%(3) = 0 then goto L50250
              goto L50270
L50210:
              custname$ = hex(06) & "Select Bill-to Customer"
              call "GETCODE" (#3, beg_cust$, custname$, 0%, 1.30, f1%(3))
              if f1%(3) = 1% then L50260
L50250:         errormsg$ = "Customer Number not on file. " & beg_cust$
L50260:
L50270:    if errormsg$ <> " " then beg_cust$ = "ALL" : return

        return

        page_head
          pcntr% = pcntr% + 1%
          print page
          print using L60150, date$, time$, company$, "APCCSAUD"
          print using L60190, rpttitle$, pcntr%
          print
          print using L60220
          print using L60250
          print
          lcntr% = 6%
        return


        REM ***********************************************************  ~
            *             I M A G E   S T A T E M E N T S             *  ~
            ***********************************************************

*       * HEADER LINE 1
L60150: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:RPTID

*       * HEADER LINE 2
L60190: %                                     ###########################~
        ~#################################                     PAGE: ####

L60220: % CUSTOMER ACCT   CHECK NUMBER   SETTLEMENT/INVOICE   ERROR MESSA~
        ~GE

L60250: % -------------   ------------   ------------------   -----------~
        ~---------------------------------------

L60280: %   #########       ########        ############      ###########~
        ~#######################################
L60300: %  N O   E R R O R S   F O U N D   D U R I N G   V E R I F I C A ~
        ~T I O N
        end_routine

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            if pcntr% <> 0% then goto L65160
               gosub page_head
               print
               print using L60300

L65160:     call "SHOSTAT" ("One Moment Please")
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            if arc_buf% = 0% then end
               call "FILEBGON" addr(#20)
            end
