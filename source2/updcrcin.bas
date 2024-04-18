        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  U   U  PPPP   DDDD    CCC   RRRR    CCC   IIIII  N   N   *~
            *  U   U  P   P  D   D  C      R   R  C        I    NN  N   *~
            *  U   U  PPPP   D   D  C      RRRR   C        I    N N N   *~
            *  U   U  P      D   D  C      R  R   C        I    N  NN   *~
            *   UUU   P      DDDD    CCC   R   R   CCC   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDCRCIN - Provides all session management functions for  *~
            *            Cash Receipts Entry (with the exception of     *~
            *            releasing for update).                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/19/86 ! Original                                 ! ERN *~
            * 10/02/87 ! Insure no short record in SYSFILE2       ! HES *~
            * 02/14/90 ! Rounded amounts before printing. Also    ! JDH *~
            *          !  added optional print of transaction amt.!     *~
            * 03/06/92 ! Mod % stmnt at 17240 for DEC compatibility MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actl_hdr$10,                 /* Actual Figures Heading     */~
            actual(6), actual$(6)10,     /* Actual Figures             */~
            aracct$12,                   /* Dflt A/R Account           */~
            aracctdescr$30,              /*                            */~
            audit$(6)10,                 /* Audit Figures              */~
            cashacct$12,                 /* Cash in Bank Account       */~
            cashacctdescr$30,            /*                            */~
            chkdate$8,                   /* Check Date                 */~
            chknr$8,                     /*       Number               */~
            chkuser$3,                   /*       User ID              */~
            company$60,                  /* Company Name               */~
            curr$4,                      /* Multi-currency on?         */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Number            */~
            cusname$30,                  /* Customer Name              */~
            date$8,                      /* Date for screen display    */~
            descr$20,                    /* Session Description        */~
            dflts$(5)12,                 /* Default Accounts           */~
            discacct$12,                 /* Cash Discounts Account     */~
            discacctdescr$30,            /*                            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            glpost$1,                    /* 'POSTME' Option (Y/N)      */~
            hdr1$7, hdr2$8, hdr3$20,     /* Summary Screen Headings    */~
            hdr4$10,                     /*       ditto                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            ina(5),                      /* In Amounts for Check       */~
            inb(5),                      /* In Amounts - Trans Currency*/~
            lastuser$3,                  /* Last User to change Session*/~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pfd$(3)79, pf$(20)1,         /* PF Descriptors and Keys    */~
            plowkey$50,                  /* Multipurpose Plow Key      */~
            postdate$8,                  /* Posting Date               */~
            postdatedescr$32,            /* Posting Date               */~
            print$1,                     /* Print Deposit Slip?        */~
            rpt(6),                      /* Report Totals              */~
            rpt_user$3,                  /* Audit- User ID or ALL      */~
            salesacct$12,                /* Dflt Sales Account         */~
            salesacctdescr$30,           /*                            */~
            session$6,                   /* Session ID                 */~
            stat$4,                      /* Statutory currency         */~
            status$10,                   /* Session Status             */~
            summary$(100)79,             /* Lines for Summary Screen   */~
            time$8,                      /* Report Run Time            */~
            trans$4,                     /* Transaction currency       */~
            unalacct$12,                 /* Unallowed Discounts Acct   */~
            unalacctdescr$30,            /*                            */~
            upd$8,                       /* Update Program Controlled  */~
            upddescr$30,                 /* Update Description         */~
            userid$3                     /* Current User Id            */

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
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! UPDSESSN ! Update Session Control File              *~
            * #2  ! SYSFILE2 ! System File                              *~
            * #3  ! CUSTOMER ! Customer Master                          *~
            * #4  ! GLMAIN   ! G/L Chart of Accounts                    *~
            * #9  ! CRCBUFFR ! Check Buffer                             *~
            * #22 ! CRCMSCUR ! Multi-Currency Master Information        *~
            *************************************************************~

            select #1,  "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1, keylen = 20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #9,  "CRCBUFFR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 201, keylen =  23

            select #22, "CRCMSCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

            call "OPENCHCK" (#1,  fs%(1), f2%(1), 100%, rslt$(1))
            call "OPENCHCK" (#2,  fs%(2), f2%(2),   0%, rslt$(2))
            call "OPENCHCK" (#3,  fs%(3), f2%(3),   0%, rslt$(3))
            call "OPENCHCK" (#4,  fs%(4), f2%(4),   0%, rslt$(4))
            call "OPENCHCK" (#9,  fs%(9), f2%(9),   0%, rslt$(9))
            call "OPENCHCK" (#22, fs%(22), f2%(22), 0%, rslt$(22))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "Position Cursor and Press RETURN to Manage" &~
                           " Data for a Session."

*        Define Headings for Summary Screen
            hdr1$ = "  ID"
            hdr2$ = "  Date"
            hdr3$ = "Session Description"
            hdr4$ = "  Status"

*        Determine maximum number of sessions allowed.
            maxsessions% = dim(summary$(), 1)

*        Define what Update is being controlled here
            upd$      = "CRCUPDTE"
            upddescr$ = "Cash Receipts Entry"

*        Load Account Defaults
            readkey$ = "ARM.CASH.ACCOUNTS"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then L10000
                get #2 using L09410, dflts$()
L09410:              FMT XX(20), 5*CH(9)

*        Check for Multi-Currency
            curr$ = "N"
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
               if f1%(2) = 0% then L10000
            get #02 using L09480, curr$, stat$
L09480:         FMT POS(21), CH(1), CH(4)

L10000: REM *************************************************************~
            *              S U M M A R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            * Shows summary of sessions.                                *~
            *************************************************************

        summary_screen
            init(" ") errormsg$, inpmessage$
            gosub load_summary
            if summary% = 0% then goto add_session

L10110:     gosub'101
                if keyhit%  =  2 then top% = 0%
                if keyhit%  =  3 then top% = max(0%,summary%-15%)
                if keyhit%  =  4 then top% = max(0%,top%-15%)
                if keyhit%  =  5 then                                    ~
                     top% = min(top%+15%, max(0%,summary%-15%))
                if keyhit%  =  6 then top% = max(0%,top%-1%)
                if keyhit%  =  7 then                                    ~
                     top% = min(top%+1%, max(0%,summary%-15%))
                if keyhit%  <  8 then errormsg$ = " "
                if keyhit% <> 11 then L10250
                     if summary% < maxsessions% then goto add_session
                          errormsg$ = "Session Queue Full."
                          goto L10110
L10250:         if keyhit%  = 16 then exit_program
                if keyhit% <>  0 then L10110

            fieldnr% = cursor%(1) - 5% + top%
            if fieldnr% < 1% or fieldnr% > summary% then L10110
            session$ = str(summary$(fieldnr%),11,6)
            gosub load_session
            if f1%(1) = 1% then edit_session
                errormsg$ = "Session not found on file."
                goto L10110


        REM *************************************************************~
            *         I N P U T   S E S S I O N   D A T A               *~
            *-----------------------------------------------------------*~
            * Handles operation of Adding a new Session.                *~
            *************************************************************
        add_session
            init(" ") errormsg$, inpmessage$, session$,                  ~
                      descr$, postdate$, postdatedescr$, lastuser$,      ~
                      status$, actl_hdr$, actual$(), audit$(), print$,   ~
                      cashacct$, discacct$, unalacct$, aracct$,          ~
                      salesacct$, cashacctdescr$, discacctdescr$,        ~
                      unalacctdescr$, aracctdescr$, salesacctdescr$
            glpost$ = "Y"

            for fieldnr% = 1% to 10%
L11150:         gosub'052(fieldnr%, 1%)
                      if enabled% = 0 then L11270
L11170:         gosub'102(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11250
L11200:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%, 1%)
                         if enabled% = 1% then L11170
                         if fieldnr% = 1% then L11150
                         goto L11200
L11250:               if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L11170
L11270:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L11170
            next fieldnr%

        REM *************************************************************~
            *        E D I T   S E S S I O N   D A T A                  *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for Session Data.          *~
            *************************************************************

        edit_session
L12070:     gosub'102(0%, 2%)
                  if keyhit%  =  1 and  (str(status$,,1) = "V" or        ~
                                         str(status$,,1) = "U")          ~
                                         then summary_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  6 then gosub close_session
                  if keyhit%  =  7 then gosub reopen_session
                  if keyhit%  =  8 then gosub delete_session
                  if keyhit%  =  9 then gosub audit_report
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 23 then gosub post_option
                  if keyhit%  = 32 then gosub save_defaults
                  if keyhit% <>  0 then       L12070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% > 9% then fieldnr% = 10%    /* Audit Figures  */
            if fieldnr% < 2% or   fieldnr% > 10% then L12070

            gosub'052(fieldnr%, 2%)
                  if enabled% = 0% then       L12070
L12260:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12260
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12260
            goto L12070


        post_option  /* Allows user to specify no G/L Postings         */
            if glpost$ = "N" then L12460
                u3% = 2%                 /* Change from 'Y' to 'N'     */
                call "ASKUSER" (u3%, "ALTER G/L POSTINGS",               ~
                               "Press PF-16 to NOT Post Session to G/L", ~
                               "- OR -", "Press RETURN to abort change.")
                if u3% <> 16% then return
                     glpost$ = "N"
L12420:              call "DATUNFMT" (postdate$)
                     gosub describe_period
                     return

L12460:         u3% = 2%                 /* Change from 'N' to 'Y'     */
                call "ASKUSER" (u3%, "ALTER G/L POSTINGS",               ~
                               "Press PF-16 to Post Session to G/L",     ~
                               "- OR -", "Press RETURN to abort change.")
                if u3% <> 16% then return
                     glpost$ = "Y"
                     goto L12420


        save_defaults
            dflts$(1) = cashacct$   :  call "GLUNFMT" (dflts$(1))
            dflts$(2) = discacct$   :  call "GLUNFMT" (dflts$(2))
            dflts$(3) = unalacct$   :  call "GLUNFMT" (dflts$(3))
            dflts$(4) = aracct$     :  call "GLUNFMT" (dflts$(4))
            dflts$(5) = salesacct$  :  call "GLUNFMT" (dflts$(5))
            readkey$  = "ARM.CASH.ACCOUNTS"
            call "READ101" (#2, readkey$, f1%(2))
                if f1%(2) <> 0% then delete #2
            write #2 using L12640, readkey$, dflts$(), " ", " "
L12640:         FMT CH(20), 5*CH(9), CH(235), CH(200)
            return


        REM *************************************************************~
            *             C L O S E   S E S S I O N                     *~
            *-----------------------------------------------------------*~
            * Verify audit figures and, if OK, change status.           *~
            *************************************************************
        close_session

*        First make sure no one's still posting to Session
            plowkey$ = hex(ffffff) & str(upd$) & str(session$) & hex(00)
            call "PLOWALTS" (#1, plowkey$, 1%, 17%, f1%(1))
            if f1%(1) = 0% then L12830
                errormsg$ = "Data Entry is still in Progress."
                return

L12830
*        Now flag session as in Verification Process
            plowkey$ = str(upd$) & str(session$)
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then L12910
                put #1 using L12880, "V"
L12880:              FMT POS(47), CH(1)
                rewrite #1

L12910
*        Now check that actuals and control totals match
            call "SHOSTAT" ("Verifying Control Totals")
            if str(audit$()) = " " then L13070     /* No Audit Controls */
            mat actual  = zer
            plowkey$ = str(session$) & hex(00)

L12970:     call "PLOWALTS" (#9, plowkey$, 1%, 6%, f1%(9))
            if f1%(9) = 0% then L13070
                actual(1) = actual(1) + 1          /* Document Count   */
                get #9 using L13010, net, disc, unal, gl
L13010:              FMT POS(40), 4*PD(14,4)
                actual(2) = actual(2) + net
                actual(3) = actual(3) + disc + unal
                actual(4) = actual(4) + gl
                goto L12970

L13070:     errs% = 0%
            for n% = 1% to 6%
                if audit$(n%) = " " then L13120
                     convert audit$(n%) to temp, data goto L13110
L13110:              if temp <> actual(n%) then errs% = errs% + 1%
L13120:     next n%


*        If we have an imbalance (ERRS% > 0%) then we need to remove the
*        verification status and inform the User of the imbalance.
            if errs% = 0% then L13330
                for n% = 1% to 6%
                     call "CONVERT" (actual(n%), 2.2, actual$(n%))
                     if n% = 1% then str(actual$(1),8) = " "
                next n%
                actl_hdr$ = "Entered:  "
                errormsg$ = "Cannot Close Session -- Entries do not"  &  ~
                            " match Control Figures"
                plowkey$ = str(upd$) & str(session$)
                call "READ101" (#1, plowkey$, f1%(1))
                if f1%(1) = 0% then return
                     put #1 using L13290, "D"
L13290:                   FMT POS(47), CH(1)
                     rewrite #1
                     return

L13330
*        No errors encountered.  Save session with Status = Closed.
            status$ = "C"
            gosub dataput
            return clear all
            goto summary_screen


        REM *************************************************************~
            *             R E - O P E N   S E S S I O N                 *~
            *-----------------------------------------------------------*~
            * Reset status from 'C'losed to 'D'ata Entry                *~
            *************************************************************
        reopen_session
            if str(status$,,1) = "C" then status$ = "Data Entry" else    ~
                errormsg$ = "Can only reopen a session that is closed."
            return


        REM *************************************************************~
            *             D E L E T E   S E S S I O N                   *~
            *-----------------------------------------------------------*~
            * Remove Session only if no postings are entered.           *~
            *************************************************************
        delete_session
            plowkey$ = str(session$) & hex(00)
            call "PLOWALTS" (#9, plowkey$, 1%, 6%, f1%(9))
            if f1%(9) = 0% then L15120
                errormsg$ = "There are still entries under this" &       ~
                            " session.  Can't delete."
                return
L15120:     plowkey$ = hex(ffffff) & str(upd$) & str(session$)
            call "PLOWALTS" (#1, plowkey$, 1%, 17%, f1%(1))
            if f1%(1) = 0% then L15180
                errormsg$ = "There are still Users entering data" &      ~
                            " into the session. Can't delete."
                return
L15180:     plowkey$ = str(upd$) & str(session$)
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE SESSION?",                      ~
                            "Enter PF-16 to DELETE this session", "-OR-",~
                            "Press RETURN to Abort Delete Function.")
            if u3% <> 16% then return
                call "DELETE" (#1, plowkey$, 14%)
                return clear all
                goto summary_screen

        REM *************************************************************~
            *                 A U D I T   R E P O R T                   *~
            *-----------------------------------------------------------*~
            * List entries for this session.                            *~
            *************************************************************
        audit_report
            init(" ") errormsg$, inpmessage$, rpt_user$

*        Input Mode for Report Parameters
            for fieldnr% = 1% to 1%
                gosub'053(fieldnr%, 1%)
                      if enabled% = 0 then L16150
L16120:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1 then exit_report
                      if keyhit% <>  0 then L16120
L16150:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L16120
            next fieldnr%

*        Edit Mode for Report Parameters
L16200:     gosub'103(0%, 2%)
                  if keyhit%  =  1 then exit_report
                  if keyhit%  = 16 then print_report
                  if keyhit% <>  0 then L16200
            fieldnr% = cursor%(1) - 8%
            if fieldnr% < 1% or   fieldnr% > 1% then L16200

            gosub'053(fieldnr%, 2%)
                  if enabled% = 0% then       L16200
L16300:     gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1 then exit_report
                  if keyhit% <>  0 then L16300
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L16300
            goto L16200

        exit_report
            errormsg$ = " "
            return

        print_report
            if curr$ <> "Y" then L16420
                print_trans% = 0% : u3% = 2%
                call "ASKUSER" (u3%, "DECISION TIME", "Do you want to " &~
                           "have the foreign amounts printed too?",      ~
                           "-PF 8- to print foreign amounts also",       ~
                           "Any other key to NOT print foreign amounts.")
                if u3% = 8% then print_trans% = 1%

L16420:     call "SHOSTAT" ("Print Cash Receipts Audit Listing...")
            mat rpt = zer
            plowkey$ = str(session$) & hex(00)
            page% = 0%  :  line% = 857%
            call "COMPNAME" (12%, company$, u3%)
            call "TIME" (time$)
            call "SETPRNT" ("CRC001", " ", 0%, 0%)
            select printer(134)

        report_loop
            call "PLOWALTS" (#9, plowkey$, 1%, 6%, f1%(9))
            if f1%(9) = 0% then end_report

            get #9 using L16610, cuscode$, chknr$, chkdate$,              ~
                                ina(4), ina(2), ina(3), ina(5), chkuser$
L16610:         FMT CH(9), CH(8), CH(6), POS(40), 4*PD(14,4), POS(99),   ~
                    CH(3)

            for i% = 2% to 5%
                ina(i%) = round(ina(i%), 2)
            next i%

            if curr$ <> "Y" or print_trans% <> 1% then L16660
                call "READ100" (#22, key(#9), f1%(22))
                if f1%(22) = 0% then L16660
                get #22 using L16650, trans$,inb(4), inb(2), inb(3), inb(5)
L16650:               FMT CH(4), POS(44), 4*PD(14,4)
                inb(1) = inb(4) - (inb(2) + inb(3)) - inb(5)

L16660:     if rpt_user$ <> "ALL" and chkuser$ <> rpt_user$              ~
                                                        then report_loop

            call "DATEFMT" (chkdate$)
            call "DESCRIBE" (#3, cuscode$, cusname$, 0%, f1%(3))
            ina(1) = ina(4) - (ina(2) + ina(3)) - ina(5)
            if line% > 55% then gosub report_heading
            print using L17240, cuscode$, cusname$, chknr$, chkdate$,     ~
                               chkuser$, ina(1), ina(2), ina(3), ina(5), ~
                               ina(4)
            line% = line% + 1%

            if f1%(22) = 0% or print_trans% <> 1% then L16770
            print using L17250, " ", " ", " ", " ", trans$, inb(1),       ~
                               inb(2), inb(3), inb(5), inb(4)
            line% = line% + 1%

L16770:     for t% = 1% to 5%
                rpt(t%) = rpt(t%) + ina(t%)
            next t%
            rpt(6) = rpt(6) + 1
            goto report_loop

        end_report
            if line% > 53% then gosub report_heading                     ~
                           else print using L17270
            print using L17300, rpt(6), rpt(1), rpt(2), rpt(3), rpt(5),   ~
                                       rpt(4)
            if curr$ <> "Y" or print_trans% <> 1% then L16880
                print using L17350
L16880:     print using L17330
            call "SETPRNT" ("CRC001", " ", 0%, 1%)
            close printer
            return


        report_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L17060, date$, time$, company$
            print using L17090, postdate$, session$, descr$, page%
            print using L17120, rpt_user$
            print
            if print_trans% <> 1% then print using L17150                 ~
                                  else print using L17160
            if print_trans% <> 1% then print using L17180                 ~
                                  else print using L17190
            print using L17210
            return


L17060: %RUN DATE: ######## ########             ########################~
        ~######################################             UPDCRCIN-CRC00~
        ~1
L17090: %    POST: ########                      CASH RECEIPTS AUDIT FOR ~
        ~SESSION ###### ####################                      PAGE: ##~
        ~#
L17120: %REPORT FOR USER: ###


L17150: %                                          CHECK    CHECK        ~
        ~    ACCOUNTS      ALLOWED     UNALLOWED   DIR. SALES
L17160: %                                          CHECK    CHECK   USER/~
        ~    ACCOUNTS      ALLOWED     UNALLOWED   DIR. SALES

L17180: %CUSTOMER  CUSTOMER NAME                   NUMBER    DATE   USER ~
        ~   RECEIVABLE    DISCOUNTS    DISCOUNTS   & G/L AMTS    NET CHECK
L17190: %CUSTOMER  CUSTOMER NAME                   NUMBER    DATE   CURR ~
        ~   RECEIVABLE    DISCOUNTS    DISCOUNTS   & G/L AMTS    NET CHECK

L17210: %--------- ------------------------------ -------- -------- ---- ~
        ~  -----------  -----------  -----------  -----------  -----------

L17240:    %######### ############################## ######## ########  #~
        ~##  -########.## -########.## -########.## -########.## -########~
        ~.##
L17250:    %######### ############################## ######## ########  #~
        ~##  -########.##*-########.##*-########.##*-########.##*-########~
        ~.##*
L17270: %                                                                ~
        ~  -----------  -----------  -----------  -----------  -----------

L17300: %                               ** SESSION TOTALS (#### CHECKS)  ~
        ~ -########.## -########.## -########.## -########.## -########.##

L17330: %   *** END OF REPORT ***

L17350: %                                - Totals are in Statutory Curren~
        ~cy -

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto  summary_screen

        REM *************************************************************~
            *    D E F A U L T / E N A B L E   F O R   S E S S I O N    *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Session Data.        *~
            *************************************************************
        deffn'052(fieldnr%, edit%)
            enabled% = 1%
            if str(status$,,1) = "D" or fieldnr% < 3% then L21130
                errormsg$ = "Cannot modify data when Session Status is " ~
                            & status$
                enabled%  = 0%
                return

L21130:           on fieldnr% gosub L21250,         /* Session ID       */~
                                    L21300,         /* Session Descr    */~
                                    L21340,         /* Posting Date     */~
                                    L21380,         /* Print Dpst Slip? */~
                                    L21420,         /* Cash Acct        */~
                                    L21500,         /* Discounts Acct   */~
                                    L21580,         /* Unallowed Acct   */~
                                    L21660,         /* A/R Acct         */~
                                    L21750,         /* Sales Account    */~
                                    L21850          /* Audit Figures    */
                     return

L21250
*        Default/Enable for SESSION ID
            if edit% = 2% then enabled% = 0%
            inpmessage$ = "Enter Session Identifier."
            return

L21300
*        Default/Enable for SESSION DESCRIPTION
            inpmessage$ = "Enter Session Description."
            return

L21340
*        Default/Enable for POSTING DATE
            inpmessage$ = "Enter G/L Posting Data for this Session."
            return

L21380
*        Default/Enable for PRINT DEPOSIT SLIP
            inpmessage$ = "Enter 'Y' to have a Deposit Slip printed."
            return

L21420
*        Default/Enable for CASH IN BANK ACCOUNT
            inpmessage$ = "Enter Cash in Bank Account Number."
            if dflts$(1) = " " or cashacct$ <> " " then return
                cashacct$ = dflts$(1)
                call "DESCRIBE" (#4, cashacct$, cashacctdescr$, 0%, u3%)
                call "GLFMT" (cashacct$)
                return

L21500
*        Default/Enable for CASH DISCOUNTS ACCOUNT
            inpmessage$ = "Enter Cash Discounts Account Number."
            if dflts$(2) = " " or discacct$ <> " " then return
                discacct$ = dflts$(2)
                call "DESCRIBE" (#4, discacct$, discacctdescr$, 0%, u3%)
                call "GLFMT" (discacct$)
                return

L21580
*        Default/Enable for UNALLOWED DISCOUNTS ACCOUNT
            inpmessage$ = "Enter Unallowed Discounts Account Number."
            if dflts$(3) = " " or unalacct$ <> " " then return
                unalacct$ = dflts$(3)
                call "DESCRIBE" (#4, unalacct$, unalacctdescr$, 0%, u3%)
                call "GLFMT" (unalacct$)
                return

L21660
*        Default/Enable for DEFAULT A/R ACCOUNT
            inpmessage$ = "Enter Default A/R Account for Unapplied" &    ~
                          " Payments."
            if dflts$(4) = " " or aracct$ <> " " then return
                aracct$ = dflts$(4)
                call "DESCRIBE" (#4, aracct$, aracctdescr$, 0%, u3%)
                call "GLFMT" (aracct$)
                return

L21750
*        Default/Enable for DEFAULT SALES ACCOUNT
            inpmessage$ = "Enter Default Sales Account for Direct"  &    ~
                          " Sales entries."
            if dflts$(5) = " " or salesacct$ <> " " then return
                salesacct$ = dflts$(5)
                call "DESCRIBE" (#4, salesacct$, salesacctdescr$, 0%, u3%)
                call "GLFMT" (salesacct$)
                return
            return

L21850
*        Default/Enable for AUDIT FIGURES
            inpmessage$ = "Enter Audit Target Figures for control"   &   ~
                          " (leave blank for no auditing)."
            return


        REM *************************************************************~
            *    D E F A U L T / E N A B L E   F O R   R E P O R T      *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Audit Report.        *~
            *************************************************************
        deffn'053(fieldnr%, edit%)
            enabled% = 1%

                  on fieldnr% gosub L22120          /* User ID          */
                     return

L22120
*        Default/Enable for USER ID
            inpmessage$ = "Enter Specific User ID -or- 'ALL'."
            if rpt_user$ = " " then rpt_user$ = "ALL"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            goto summary_screen

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_summary
            summary%, top% = 0%
            init(" ") summary$()
            plowkey$ = str(upd$) & hex(00)

L30100:     call "PLOWALTS" (#1, plowkey$, 0%, 8%, f1%(1))
            if f1%(1) = 0% then return
            get #1 using L30130, lastuser$   /* Dummy usage   */
L30130:         FMT CH(3)
            if lastuser$ = hex(ffffff) then L30100
                s%, summary% = summary% + 1%
                get #1 using L30220,                                      ~
                          str(summary$(s%),11, 6),       /* Session    */~
                          str(summary$(s%),30,20),       /* Descriptn  */~
                          str(summary$(s%),20, 6),       /* Post Date  */~
                          str(summary$(s%),52, 1),       /* Status     */~
                          glpost$                        /* Post Optn  */
L30220:              FMT XX(11), CH(6), XX(3), CH(20), CH(6), CH(1),     ~
                         XX(3), CH(1)
                call "DATEFMT" (str(summary$(s%),20, 8))
                if str(summary$(s%),52,1) = "C" then                     ~
                                   str(summary$(s%),52,10) = "Closed"
                if str(summary$(s%),52,1) = "D" then                     ~
                                   str(summary$(s%),52,10) = "Data Entry"
                if str(summary$(s%),52,1) = "V" then                     ~
                                   str(summary$(s%),52,10) = "Verifying "
                if str(summary$(s%),52,1) = "U" then                     ~
                                   str(summary$(s%),52,10) = "Updating  "
                if glpost$ <> "Y" then                                   ~
                                str(summary$(s%),64) = "-NO G/L POSTING-"
                goto L30100


        REM *************************************************************~
            *           L O A D   S E S S I O N   D A T A               *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_session
            init(" ") errormsg$, inpmessage$,                            ~
                      descr$, postdate$, postdatedescr$, actl_hdr$,      ~
                      actual$()
            lastuser$ = userid$
            status$   = "Data Entry"
            glpost$   = "Y"

            plowkey$ = str(upd$) & str(session$)
            call "READ100" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then return
               get #1 using L30580, descr$, postdate$, status$, lastuser$,~
                                   glpost$, print$, audit$(), cashacct$, ~
                                   discacct$, unalacct$, aracct$,        ~
                                   salesacct$
L30580:              FMT XX(20), CH(20), CH(6), CH(1), CH(3), CH(1),     ~
                         CH(1), XX(26), 6*CH(10), 5*CH(9)
              /* Decipher data elements so they mean something.        */
                gosub describe_period
                if str(status$,,1) = "C" then status$ = "Closed"
                if str(status$,,1) = "D" then status$ = "Data Entry"
                if str(status$,,1) = "V" then status$ = "Verifying"
                if str(status$,,1) = "U" then status$ = "Updating"
                call "DESCRIBE" (#4, cashacct$ , cashacctdescr$ , 0%, u%)
                call "DESCRIBE" (#4, discacct$ , discacctdescr$ , 0%, u%)
                call "DESCRIBE" (#4, unalacct$ , unalacctdescr$ , 0%, u%)
                call "DESCRIBE" (#4, aracct$   , aracctdescr$   , 0%, u%)
                call "DESCRIBE" (#4, salesacct$, salesacctdescr$, 0%, u%)
                call "GLFMT" (cashacct$ )
                call "GLFMT" (discacct$ )
                call "GLFMT" (unalacct$ )
                call "GLFMT" (aracct$   )
                call "GLFMT" (salesacct$)
                if str(status$,,1) = "V" or str(status$,,1) = "U" then   ~
                                                                  return
                if month% <> 0% then return
                     errormsg$ = "Post Date Invalid -- Please Correct"
                     return clear all
                     fieldnr% = 3%
                     goto L12260      /* In the middle of Edit Mode */


        describe_period   /* Describe G/L Posting Period     */
            call "WHICHPER" (#2, postdate$, glper%)
            call "WHICHMON" (#2, postdate$, month%)
            call "DATEFMT" (postdate$)
            if glpost$ = "Y" then L30920
                postdatedescr$ = "** NO G/L POSTING **"
                return
L30920:     convert glper% to postdatedescr$, pic(#0)
            postdatedescr$ = "Month (Period: " & postdatedescr$ & ")"
            on month% goto L30980, L30970, L30960
                postdatedescr$ = "INVALID " & postdatedescr$ : return
L30960:         postdatedescr$ = "Next "    & postdatedescr$ : return
L30970:         postdatedescr$ = "Current " & postdatedescr$ : return
L30980:         postdatedescr$ = "Prior "   & postdatedescr$ : return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUNFMT" (postdate$)
            call "GLUNFMT" (cashacct$ )
            call "GLUNFMT" (discacct$ )
            call "GLUNFMT" (unalacct$ )
            call "GLUNFMT" (aracct$   )
            call "GLUNFMT" (salesacct$)
            plowkey$ = str(upd$) & str(session$)
            call "READ101" (#1, plowkey$, f1%(1))
                if f1%(1) <> 0% then delete #1
            write #1 using L31190, "   ", upd$, session$, " ", descr$,    ~
                                  postdate$, str(status$,,1), userid$,   ~
                                  glpost$, print$, " ", audit$(),        ~
                                  cashacct$, discacct$, unalacct$,       ~
                                  aracct$, salesacct$, " "
L31190:         FMT CH(3), CH(8), CH(6), CH(3), CH(20), CH(6), CH(1),    ~
                    CH(3), CH(1), CH(1), CH(26), 6*CH(10), 5*CH(9),      ~
                    CH(17)
            return

        REM *************************************************************~
            *               S U M M A R Y   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Summary Screen.                                           *~
            *************************************************************

        deffn'101
            line2$ = "  ** SUMMARY **"
            str(line2$,62) = "UPDCRCIN: " & str(cms2v$,,8)
            init(hex(86)) lfac$()
            gosub setpf_summary
            if inpmessage$ = " " then inpmessage$ = edtmessage$


L40140:     accept                                                       ~
               at (01,02), "Session Management for ",                    ~
               at (01,25), fac(hex(84)), upddescr$              , ch(30),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,12), "Session   Posting",                          ~
               at (05,12), fac(hex(ac)), hdr1$,                          ~
               at (05,21), fac(hex(ac)), hdr2$,                          ~
               at (05,31), fac(hex(ac)), hdr3$,                          ~
               at (05,53), fac(hex(ac)), hdr4$,                          ~
               at (06,02), fac(hex(80)),   summary$(top% +  1%) , ch(79),~
                                                                         ~
               at (06,02), fac(lfac$( 1)), summary$(top% +  1%) , ch(79),~
               at (07,02), fac(lfac$( 2)), summary$(top% +  2%) , ch(79),~
               at (08,02), fac(lfac$( 3)), summary$(top% +  3%) , ch(79),~
               at (09,02), fac(lfac$( 4)), summary$(top% +  4%) , ch(79),~
               at (10,02), fac(lfac$( 5)), summary$(top% +  5%) , ch(79),~
               at (11,02), fac(lfac$( 6)), summary$(top% +  6%) , ch(79),~
               at (12,02), fac(lfac$( 7)), summary$(top% +  7%) , ch(79),~
               at (13,02), fac(lfac$( 8)), summary$(top% +  8%) , ch(79),~
               at (14,02), fac(lfac$( 9)), summary$(top% +  9%) , ch(79),~
               at (15,02), fac(lfac$(10)), summary$(top% + 10%) , ch(79),~
               at (16,02), fac(lfac$(11)), summary$(top% + 11%) , ch(79),~
               at (17,02), fac(lfac$(12)), summary$(top% + 12%) , ch(79),~
               at (18,02), fac(lfac$(13)), summary$(top% + 13%) , ch(79),~
               at (19,02), fac(lfac$(14)), summary$(top% + 14%) , ch(79),~
               at (20,02), fac(lfac$(15)), summary$(top% + 15%) , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L40540
                  call "MANUAL" ("UPDCRCIN")
                  goto L40140

L40540:        if keyhit% <> 15 then L40580
                  call "PRNTSCRN"
                  goto L40140

L40580:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_summary
         pfd$(1) = "                                        "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "(2)First (4)Prev (6)Down    (11)Add a Se"   &        ~
                   "ssion                  (15)Print Screen"
         pfd$(3) = "(3)Last  (5)Next (7)Up                  "   &        ~
                   "                       (16)Exit Program"
            str(pf$()) = hex(000203040506070b0d0f10ffffffffffffffffff)
            if top% <> 0% then L40700
               str(pfd$(2),,25) = " "  :  pf$(2), pf$(4), pf$(6) = hex(ff)
L40700:     if top% + 15% < summary% then return
               str(pfd$(3),,25) = " "  :  pf$(3), pf$(5), pf$(7) = hex(ff)
               return

        REM *************************************************************~
            *        I N P U T  / E D I T   S E S S I O N               *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            line2$ = "  Manage Session Data"
            str(line2$,62) = "UPDCRCIN: " & str(cms2v$,,8)
            init(hex(8c)) lfac$()
            if fieldnr% = 0% then init(hex(86)) lfac$()
            if fieldnr% = 0% then init(hex(8c)) lfac$(1)
            if edit% = 1% then gosub setpf_input else gosub setpf_edit

                  on fieldnr% gosub L41290,         /* Session ID       */~
                                    L41260,         /* Session Descr    */~
                                    L41290,         /* Posting Date     */~
                                    L41290,         /* Print Dpst Slip? */~
                                    L41290,         /* Cash Acct        */~
                                    L41290,         /* Discounts Acct   */~
                                    L41290,         /* Unallowed Acct   */~
                                    L41290,         /* A/R Acct         */~
                                    L41290,         /* Sales Account    */~
                                    L41320          /* Audit Figures    */
                  goto L41360

L41260:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L41290:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L41320:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L41360:     accept                                                       ~
               at (01,02), "Session Management for ",                    ~
               at (01,25), fac(hex(84)), upddescr$              , ch(30),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session ID",                                 ~
               at (06,30), fac(lfac$( 1)), session$             , ch(06),~
               at (06,53), "Status: ",                                   ~
               at (06,60), fac(hex(84))  , status$              , ch(10),~
                                                                         ~
               at (07,02), "Session Description",                        ~
               at (07,30), fac(lfac$( 2)), descr$               , ch(20),~
               at (07,53), "Last Modified By:",                          ~
               at (07,70), fac(hex(8c))  , lastuser$            , ch(03),~
                                                                         ~
               at (08,02), "Posting Date",                               ~
               at (08,30), fac(lfac$( 3)), postdate$            , ch(08),~
               at (08,53), fac(hex(8c)),   postdatedescr$       , ch(28),~
                                                                         ~
               at (09,02), "Print Deposit Slip? (Y/N)",                  ~
               at (09,30), fac(lfac$( 4)), print$               , ch(01),~
                                                                         ~
               at (10,02), "Cash in Bank Account",                       ~
               at (10,30), fac(lfac$( 5)), cashacct$            , ch(12),~
               at (10,49), fac(hex(8c))  , cashacctdescr$       , ch(30),~
                                                                         ~
               at (11,02), "Cash Discounts Allowed",                     ~
               at (11,30), fac(lfac$( 6)), discacct$            , ch(12),~
               at (11,49), fac(hex(8c))  , discacctdescr$       , ch(30),~
                                                                         ~
               at (12,02), "Cash Discounts Not Allowed",                 ~
               at (12,30), fac(lfac$( 7)), unalacct$            , ch(12),~
               at (12,49), fac(hex(8c))  , unalacctdescr$       , ch(30),~
                                                                         ~
               at (13,02), "Default A/R Account",                        ~
               at (13,30), fac(lfac$( 8)), aracct$              , ch(12),~
               at (13,49), fac(hex(8c))  , aracctdescr$         , ch(30),~
                                                                         ~
               at (14,02), "Default Sales Account",                      ~
               at (14,30), fac(lfac$( 9)), salesacct$           , ch(12),~
               at (14,49), fac(hex(8c))  , salesacctdescr$      , ch(30),~
                                                                         ~
               at (16,02), "Check (Document) Count",                     ~
               at (16,30), fac(lfac$(10)), audit$(1)            , ch(07),~
               at (16,42), fac(hex(84))  , actl_hdr$,                    ~
               at (16,52), fac(hex(84))  , actual$(1)           , ch(10),~
                                                                         ~
               at (17,02), "Net Check Amount",                           ~
               at (17,30), fac(lfac$(10)), audit$ (2)           , ch(10),~
               at (17,52), fac(hex(84))  , actual$(2)           , ch(10),~
                                                                         ~
               at (18,02), "Allowed & Unallowed Discs",                  ~
               at (18,30), fac(lfac$(10)), audit$ (3)           , ch(10),~
               at (18,52), fac(hex(84))  , actual$(3)           , ch(10),~
                                                                         ~
               at (19,02), "Direct Sales & G/L Postings",                ~
               at (19,30), fac(lfac$(10)), audit$ (4)           , ch(10),~
               at (19,52), fac(hex(84))  , actual$(4)           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L42100
                  call "MANUAL" ("UPDCRCIN")
                  goto L41360

L42100:        if keyhit% <> 15 then L42140
                  call "PRNTSCRN"
                  goto L41360

L42140:        if keyhit% <> 14 then L42200
                     plowkey$ = hex(ffffff) & str(upd$) & str(session$)  ~
                                & hex(00)
                     call "PLOWCODE" (#1, plowkey$, " ", 17%, 1.38, u3%)
                     goto L41360

L42200:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_input
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = "(4)Previous Field                       "   &        ~
                   "                       (16)Exit Program"
            str(pf$()) = hex(0001ff04ffffffff0d0f10ffffffffffffffffff)
            if summary% = 0% and fieldnr% = 1% then return
                str(pfd$(3),64) = " " : pf$(11) = hex(ff)
                return

        setpf_edit
            if str(status$,,1) = "V" or str(status$,,1) = "U" then L42540
         pfd$(1) = "(1)Start Over         (8)Delete Session "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "(6)Close Session      (9)Audit Listing  "   &        ~
                   "   (14)See Who is In   (15)Print Screen"
         pfd$(3) = "(7)Reopen Session                       "   &        ~
                   "   (32)Save Defaults   (16)Save Data   "
            str(pf$()) = hex(01ffffffff06070809ffffff0d0e0f10170020ffff)
            if fieldnr% = 0% then                                        ~
                inpmessage$ = "Position Cursor and Press Return to" &    ~
                              " Modify a displayed value."
            if str(status$,,1) <> "C" then L42500
                str(pfd$(2),  ,16) = " " : pf$(6) = hex(ff)
L42500:     if str(status$,,1) <> "D" then return
                str(pfd$(3),  ,17) = " " : pf$(7) = hex(ff)
                return

L42540:  pfd$(1) = "                                        "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = " "
            str(pf$()) = hex(ff01ffffffffff0dff0fffffffffffffffffffff)
            inpmessage$ = "Press PF-1 to return to Summary Screen."
            return


        REM *************************************************************~
            *               A U D I T   R E P R O R T                   *~
            *-----------------------------------------------------------*~
            * Run Audit Report for a Session.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            line2$ = "  Session Audit Report"
            str(line2$,62) = "UPDCRCIN: " & str(cms2v$,,8)
            init(hex(8c)) lfac$()
            if fieldnr% = 0% then init(hex(86)) lfac$() else             ~
                                  init(hex(8c)) lfac$()
            if edit% = 1% then gosub setpf_rptinp else gosub setpf_rptedt

                  on fieldnr% gosub L44210,         /* User ID          */~
                                    L44210          /* Report Types     */
                  goto L44280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L44210:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L44280:     accept                                                       ~
               at (01,02), "Session Management for ",                    ~
               at (01,25), fac(hex(84)), upddescr$              , ch(30),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session ID",                                 ~
               at (06,30), fac(hex(84)),   session$             , ch(06),~
               at (06,53), "Status: ",                                   ~
               at (06,60), fac(hex(84))  , status$              , ch(10),~
                                                                         ~
               at (07,02), "Session Description",                        ~
               at (07,30), fac(hex(84)),   descr$               , ch(20),~
               at (07,53), "Last Modified By:",                          ~
               at (07,70), fac(hex(8c))  , lastuser$            , ch(03),~
                                                                         ~
               at (09,02), "Single User ID or 'ALL'",                    ~
               at (09,36), fac(lfac$( 1)), rpt_user$            , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L44760
                  call "MANUAL" ("UPDCRCIN")
                  goto L44280

L44760:        if keyhit% <> 15 then L44800
                  call "PRNTSCRN"
                  goto L44280

L44800:        if keyhit% <> 14 then L44860
                     plowkey$ = hex(ffffff) & str(upd$) & str(session$)  ~
                                & hex(00)
                     call "PLOWCODE" (#1, plowkey$, " ", 17%, 1.38, u3%)
                     goto L44280

L44860:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_rptinp
         pfd$(1) = "(1)Exit Report                          "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = "                                        "   &        ~
                   "                                       "
            str(pf$()) = hex(0001ffffffffffff0d0fffffffffffffffffffff)
            return

        setpf_rptedt
            if fieldnr% <> 0% then L45150
         pfd$(1) = "(1)Exit Report                          "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = "                                        "   &        ~
                   "                       (16)Print Report"
            str(pf$()) = hex(0001ffffffffff0dff0f10ffffffffffffffffff)
            inpmessage$ = "Position Cursor and Press Return to" &        ~
                          " Modify a displayed value."
            return

L45150:  pfd$(1) = "(1)Exit Report                          "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = " "
            str(pf$()) = hex(0001ffffffffff0dff0fffffffffffffffffffff)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51200,         /* Session ID       */~
                                    L51280,         /* Session Descr    */~
                                    L51330,         /* Posting Date     */~
                                    L51420,         /* Print Dpst Slip? */~
                                    L51470,         /* Cash Acct        */~
                                    L51530,         /* Discounts Acct   */~
                                    L51590,         /* Unallowed Acct   */~
                                    L51650,         /* A/R Acct         */~
                                    L51710,         /* Sales Account    */~
                                    L51770          /* Audit Figures    */
                  return

L51200
*        Test Data for SESSION ID
            if session$ <> " " then L51230
                errormsg$ = "Session ID may not be left blank." : return
L51230:     gosub load_session
            if f1%(1) = 0% or errormsg$ <> " " then return
                return clear all
                goto edit_session

L51280
*        Test Data for SESSION DESCRIPTION
            if descr$ <> " " then return
                errormsg$ = "Session Description may not be left blank."
                return

L51330
*        Test Data for POSTING DATE
            call "DATEOK" (postdate$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (postdate$)
                gosub describe_period
                if month% > 0% then return
                     errormsg$ = "Outside of G/L Open Periods."
                     return

L51420
*        Test Data for PRINT DEPOSIT SLIP?
            if print$ = "Y" or print$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L51470
*        Test Data for CASH IN BANK ACCOUNT
            cashacctdescr$ = hex(06) & "Select Cash Account"
            call "GETCODE" (#4, cashacct$, cashacctdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then return
                errormsg$ = "Invalid Account Number"  :  return

L51530
*        Test Data for DISCOUNTS ACCOUNT
            discacctdescr$ = hex(06) & "Select Discounts Account"
            call "GETCODE" (#4, discacct$, discacctdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then return
                errormsg$ = "Invalid Account Number"  :  return

L51590
*        Test Data for UNALLOWED DISCS ACCT
            unalacctdescr$ = hex(06) & "Select Unallowed Account"
            call "GETCODE" (#4, unalacct$, unalacctdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then return
                errormsg$ = "Invalid Account Number"  :  return

L51650
*        Test Data for A/R ACCOUNT
            aracctdescr$ = hex(06) & "Select A/R Account"
            call "GETCODE" (#4, aracct$, aracctdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then return
                errormsg$ = "Invalid Account Number"  :  return

L51710
*        Test Data for SALES ACCOUNT
            salesacctdescr$ = hex(06) & "Select Sales Account"
            call "GETCODE" (#4, salesacct$, salesacctdescr$, 0%,0,f1%(4))
            if f1%(4) = 1% then return
                errormsg$ = "Invalid Account Number"  :  return

L51770
*        Test Data for AUDIT FIGURES
            for n% = 1% to 6%
                if audit$(n%) = " " then L51830
                     convert audit$(n%) to temp, data goto L51860
                     call "CONVERT" (temp, 2.2, audit$(n%))
                     if n% = 1% then str(audit$(1),8) = " "
L51830:     next n%
            return

L51860:     on n% goto L51870, L51880, L51890, L51900, L51910, L51920
L51870:         errormsg$ = "Invalid entry for Document Count."   : return
L51880:         errormsg$ = "Invalid entry for A/R Amount."       : return
L51890:         errormsg$ = "Invalid entry for Discounts Amount." : return
L51900:         errormsg$ = "Invalid entry for Direct Postings."  : return
L51910:         errormsg$ = "                            "        : return
L51920:         errormsg$ = "                               "     : return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Audit Report                   *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52140          /* User ID          */
                  return

L52140
*        Test Data for USER ID
            if rpt_user$ <> " " then return
                errormsg$ = "User ID may not be left blank." : return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
