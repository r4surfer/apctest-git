        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  U   U  PPPP   DDDD    AAA   RRRR   IIIII  IIIII  N   N   *~
            *  U   U  P   P  D   D  A   A  R   R    I      I    NN  N   *~
            *  U   U  PPPP   D   D  AAAAA  RRRR     I      I    N N N   *~
            *  U   U  P      D   D  A   A  R  R     I      I    N  NN   *~
            *   UUU   P      DDDD   A   A  R   R  IIIII  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDARIIN - Provides all session management functions for  *~
            *            Vendor Invoice Entry (with the exception of    *~
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
            * 03/23/88 ! Fixed Paging Bug - didn't page           ! TLJ *~
            * 02/27/90 ! Rounded amounts before printing. Also    ! JDH *~
            *          !  added optional print of transaction amt.!     *~
            *          !  Also, export orders can now be printed. !     *~
            * 09/24/90 ! Ensured export printing (PRR).  Now MC   ! JDH *~
            *          !  values are formatted via CURRFMT.       !     *~
            * 09/24/90 ! Added detail option to audit rpt. Thanks ! JDH *~
            *          !  DAW.  Also, multi-currency with detail. !     *~
            * 05/28/91 ! PRR 11970, 11688 (fix signs). ALLFREE.   ! JIM *~
            * 03/17/92 ! PRR 12302 - Add User ID to Summary Scrn. ! JDH *~
            *          ! PRR 12062 - Posting date default added.  !     *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actl_hdr$10,                 /* Actual Figures Heading     */~
            actual(6), actual$(6)10,     /* Actual Figures             */~
            artype$4,                    /* Invoice A/R Type           */~
            audit$(6)10,                 /* Audit Figures              */~
            blankdate$8,                 /* blank unfmt date           */~
            company$60,                  /* Company Name               */~
            curr$1,                      /* Is multi-currency on?      */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Number            */~
            cusname$30,                  /* Customer Name              */~
            date$8,                      /* Date for screen display    */~
            default_date$6,              /* Date for Posting Default   */~
            descr$20,                    /* Session Description        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exten$10,                    /* Line extension amount      */~
            ext_cur$11,                  /* Line extension amt - MultiC*/~
            glpost$1,                    /* 'POSTME' Option (Y/N)      */~
            hdr1$7, hdr2$8, hdr3$20,     /* Summary Screen Headings    */~
            hdr4$10, hdr5$4,             /*       ditto                */~
            hny_key$50,                  /* Read key for HNYQUAN       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            ina(5),                      /* Invoice Amounts (Statutory)*/~
            inb(5),                      /* Invoice Amounts (Trans)    */~
            inb$(5)11,                   /* Inv Amts (Trans)-Formatted */~
            invdate$8,                   /* Invoice Date               */~
            invnr$8,                     /* Invoice User               */~
            invtype$1,                   /* Invoice Type               */~
            invuser$3,                   /* Invoice User               */~
            lastuser$3,                  /* Last User to change Session*/~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line_key$50,                 /* Read key for lines buffer  */~
            lot$16,                      /* Lot Number                 */~
            part$25,                     /* Part number                */~
            part_desc$32,                /* Part description           */~
            pfd$(3)79, pf$(20)1,         /* PF Descriptors and Keys    */~
            plowkey$50,                  /* Multipurpose Plow Key      */~
            postdate$8,                  /* Posting Date               */~
            postdatedescr$32,            /* Posting Date               */~
            print_tran_prompt$30,        /* Print tran currency prompt */~
            print_tran$1,                /* Print tran currency flag   */~
            processor$6,                 /* User Updating Session Descr*/~
            processor_id$3,              /* User Updating Session      */~
            qty_ship$10,                 /* Qty shipped on this line   */~
            rpt(6),                      /* Report Totals              */~
            rpt_det$1,                   /* Print detail or summary    */~
            rpt_type$(8)1,               /* Audit- Which Types         */~
            rpt_user$3,                  /*        User ID or ALL      */~
            seqnr$3,                     /* Invoice line seqnc number  */~
            session$6,                   /* Session ID                 */~
            stat$4,                      /* Statutory currency         */~
            status$10,                   /* Session Status             */~
            store$3,                     /* Store Number               */~
            summary$(100)79,             /* Lines for Summary Screen   */~
            tax$1,                       /* Taxable flag               */~
            time$8,                      /* Report Run Time            */~
            tot_cost$10,                 /* Total line cost - HNYQUAN  */~
            tran$5,                      /* Transaction currency       */~
            type$8, types$(8)4,          /* Invoice Types Info         */~
            unit_prc$10,                 /* Unit price                 */~
            unit_cur$11,                 /* Unit price - MultiCurrency */~
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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #3  ! USERINFO ! User Default Info File                   *~
            * #9  ! ARIBUFFR ! Invoicing Buffer File                    *~
            * #10 ! ARIBUF2  ! Invoice lines buffer file                *~
            * #11 ! HNYQUAN  ! Inventory Quantity File.                 *~
            * #22 ! ARIMSCUR ! Invoice Currency Master Shadow File      *~
            * #23 ! ARILNCUR ! Currency-specific ARI lines              *~
            *************************************************************~

            select #1,  "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1, keylen = 20

            select #3,  "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen = 3

            select #9,  "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 2001, keylen =  24,         ~
                            key  2, keypos =   34, keylen =  16, dup

            select #10, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos = 1, keylen = 20

            select #11, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos = 17, keylen = 44

            select #22, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #23, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

            call "OPENCHCK" (#1,  fs%( 1), f2%( 1), 100%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#9,  fs%( 9), f2%( 9), 1000%, rslt$( 9))
            call "OPENCHCK" (#10,  fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11,  fs%(11), f2%(11),   0%, rslt$(11))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            edtmessage$  = "Position Cursor and Press RETURN to Manage" &~
                           " Data for a Session."

*        Define Headings for Summary Screen
            hdr1$ = "  ID"
            hdr2$ = "  Date"
            hdr3$ = "Session Description"
            hdr4$ = "  Status"
            hdr5$ = "  ID"

*        Determine maximum number of sessions allowed.
            maxsessions% = dim(summary$(), 1)

*        Define what Update is being controlled here
            upd$      = "ARIUPDTE"
            upddescr$ = "Customer Invoice Entry"

*        And some other misc. stuff
            type$ = "ACDFGMOX"
            types$(1) = "ADJ "
            types$(2) = "CM  "
            types$(3) = "DIR "
            types$(4) = "FC  "
            types$(5) = "GEN "
            types$(6) = "MANL"
            types$(7) = "ORDR"
            types$(8) = "XPRT"

*        Check for Multi-Currency
            curr$ = "N" : stat$ = " "
            print_tran_prompt$ = " " : print_tran$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
               if f1%(2) = 0% then L09550
            get #2 using L09520, curr$, stat$
L09520:         FMT POS(21), CH(1), CH(4)
            if curr$ = "N" then L09550
               call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))
               call "OPENCHCK" (#23, fs%(23), f2%(23),   0%, rslt$(23))
               print_tran_prompt$ = "Include Transaction Currency? "

L09550
*        Get the Receivables Default Posting Date for this user
            call "READ100" (#3, userid$, f1%(3))
            if f1%(3) = 1% then get #3 using L09580, default_date$
L09580:         FMT POS(4), CH(6)

        REM *************************************************************~
            *              S U M M A R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            * Shows summary of sessions.                                *~
            *************************************************************

        summary_screen
            init(" ") errormsg$, inpmessage$
            call "ALLFREE"
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
                if keyhit% <> 11 then L10220
                     if summary% < maxsessions% then goto add_session
                          errormsg$ = "Session Queue Full."
                          goto L10110
L10220:         if keyhit%  = 16 then exit_program
                if keyhit% <>  0 then L10110

            fieldnr% = cursor%(1) - 5% + top%
            if fieldnr% < 1% or fieldnr% > summary% then L10110
            session$ = str(summary$(fieldnr%),5,6)
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
                      status$, actl_hdr$, actual$(), audit$()
            glpost$ = "Y"

            for fieldnr% = 1% to  4%
L11120:         gosub'052(fieldnr%, 1%)
                      if enabled% = 0 then L11240
L11140:         gosub'102(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11220
L11170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%, 1%)
                         if enabled% = 1% then L11140
                         if fieldnr% = 1% then L11120
                         goto L11170
L11220:               if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L11140
L11240:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L11140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   S E S S I O N   D A T A                  *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for Session Data.          *~
            *************************************************************

        edit_session
L12070:     gosub'102(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  6 then gosub close_session
                  if keyhit%  =  7 then gosub reopen_session
                  if keyhit%  =  8 then gosub delete_session
                  if keyhit%  =  9 then gosub audit_report
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 23 then gosub post_option
                  if keyhit% <>  0 then       L12070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% = 4% or   fieldnr% = 5% then L12070
            if fieldnr% > 5% then fieldnr% = 4%    /* Audit Figures  */
            if fieldnr% < 2% or   fieldnr% > 4% then L12070

            gosub'052(fieldnr%, 2%)
                  if enabled% = 0% then       L12070
L12210:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12210
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12210
            goto L12070


        post_option  /* Allows user to specify no G/L Postings         */
            if glpost$ = "N" then L12410
                u3% = 2%                 /* Change from 'Y' to 'N'     */
                call "ASKUSER" (u3%, "ALTER G/L POSTINGS",               ~
                               "Press PF-16 to NOT Post Session to G/L", ~
                               "- OR -", "Press RETURN to abort change.")
                if u3% <> 16% then return
                     glpost$ = "N"
L12370:              call "DATUNFMT" (postdate$)
                     gosub describe_period
                     return

L12410:         u3% = 2%                 /* Change from 'N' to 'Y'     */
                call "ASKUSER" (u3%, "ALTER G/L POSTINGS",               ~
                               "Press PF-16 to Post Session to G/L",     ~
                               "- OR -", "Press RETURN to abort change.")
                if u3% <> 16% then return
                     glpost$ = "Y"
                     goto L12370



        REM *************************************************************~
            *             C L O S E   S E S S I O N                     *~
            *-----------------------------------------------------------*~
            * Verify audit figures and, if OK, change status.           *~
            *************************************************************
        close_session

*        First make sure no one's still posting to Session
            plowkey$ = hex(ffffff) & str(upd$) & str(session$) & hex(00)
            call "PLOWALTS" (#1, plowkey$, 1%, 17%, f1%(1))
            if f1%(1) = 0% then L12660
                errormsg$ = "Data Entry is still in Progress."
                return

L12660
*        Now flag session as in Verification Process
            plowkey$ = str(upd$) & str(session$)
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then L12740
                put #1 using L12710, "V"
L12710:              FMT POS(47), CH(1)
                rewrite #1

L12740
*        Now check that actuals and control totals match
            call "SHOSTAT" ("Verifying Control Totals")
            if str(audit$()) = " " then L12880     /* No Audit Controls */
            mat actual  = zer
            plowkey$ = str(session$) & hex(00)

L12800:     call "PLOWALTS" (#9, plowkey$, 1%, 6%, f1%(9))
            if f1%(9) = 0% then L12880
                actual(1) = actual(1) + 1          /* Document Count   */
                get #9 using L12840, gross, disc, frt, tax, net
L12840:              FMT POS(793), PD(14,4), XX(8), 4*PD(14,4)
                actual(2) = actual(2) + gross
                actual(3) = actual(3) + disc
                actual(4) = actual(4) + frt
                actual(5) = actual(5) + tax
                actual(6) = actual(6) + net
                goto L12800

L12880:     errs% = 0%
            for n% = 1% to 6%
                if audit$(n%) = " " then L12920
                     convert audit$(n%) to temp, data goto L12910
L12910:              if temp <> actual(n%) then errs% = errs% + 1%
L12920:     next n%


*        If we have an imbalance (ERRS% > 0%) then we need to remove the
*        verification status and inform the User of the imbalance.
            if errs% = 0% then L13120
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
                     put #1 using L13080, "D"
L13080:                   FMT POS(47), CH(1)
                     rewrite #1
                     return

L13120
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
            init(" ") errormsg$, inpmessage$, rpt_user$, rpt_type$(),    ~
                      rpt_det$, print_tran$

*        Input Mode for Report Parameters
            for fieldnr% = 1% to 4%
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
            if fieldnr% > 4% and fieldnr% < 11% then fieldnr% = 3%
            if fieldnr% = 11% then fieldnr% = 4%
            if fieldnr% < 1% or fieldnr% > 4% then L16200

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
            call "SHOSTAT" ("Print Invoice Audit Listing...")
            mat rpt = zer
            for t% = 1% to 8%   /* 8 invoice types */
                if rpt_type$(t%) <> " " then rpt_type$(t%)               ~
                                                       = str(type$,t%,1)
            next t%
            plowkey$ = str(session$) & hex(00)
            page% = 0%  :  line% = 857%
            call "COMPNAME" (12%, company$, u3%)
            time$ = " "
            call "TIME" (time$)
            call "SETPRNT" ("ARI001", " ", 0%, 0%)
            select printer(134)

        report_loop
            call "PLOWALTS" (#9, plowkey$, 1%, 6%, f1%(9))
            if f1%(9) = 0% then end_report

            get #9 using L16610, cuscode$, invnr$, cusname$, invdate$,    ~
                                invuser$,                                ~
                                ina(1), ina(2), ina(3), ina(4), ina(5),  ~
                                store$, invtype$, artype$
L16610:         FMT CH(9), CH(8), XX(35), CH(30), POS(521), CH(6),       ~
                    POS(545), CH(3), POS(793), PD(14,4), XX(8),          ~
                    4*PD(14,4), POS(870), CH(3), POS(891), CH(1),        ~
                    POS(907), CH(1)

            for i% = 1% to 5%
                ina(i%) = round(ina(i%), 2)
            next i%
            type% = pos(str(rpt_type$()) = invtype$)
            if type% = 0% then report_loop
            if rpt_user$ <> "ALL" and invuser$ <> rpt_user$              ~
                                                        then report_loop

            if curr$ <> "Y" or print_tran$ <> "Y" then L16695
                call "READ100" (#22, key(#9), f1%(22))
                if f1%(22) = 0% then L16695
                get #22 using L16686, tran$, inb(1), inb(2), inb(3),      ~
                                     inb(4), inb(5)
L16686:               FMT CH(4), POS(22), 5*PD(14,4)

                for i% = 1 to 5
                   call "CURRFMT" (inb(i%), str(tran$,,4),               ~
                                   str(inb$(i%),,10), "N")
                     str(inb$(i%), 11, 1) = "*"
                next i%
                str(tran$, 5, 1) = "*"

L16695:     call "DATEFMT" (invdate$)
            if artype$ = "A" then artype$ = "A/R"
            if artype$ = "C" then artype$ = "CASH"
            if artype$ = "E" then artype$ = "EXP"
            if line% > 54% then gosub report_heading
            print using L18500, cuscode$, cusname$, invnr$, invdate$,     ~
                               invuser$, types$(type%), artype$,         ~
                               ina(1), ina(2), ina(3), ina(4), ina(5)
            line% = line% + 1%

            if f1%(22) = 0% or print_tran$ <> "Y" then L16770
            print using L18530, tran$, inb$(1), str(inb$(2),2),           ~
                               str(inb$(3),2), str(inb$(4),2),           ~
                               inb$(5)
            line% = line% + 1%

L16770:     for t% = 1% to 5%
                rpt(t%) = rpt(t%) + ina(t%)
            next t%
            rpt(6) = rpt(6) + 1

            if rpt_det$="S" then report_loop  /* Print summary only */

            /* NOTE - We are only getting the first lot number */
            line_key$ = all(hex(00))
            str(line_key$,1,17) = str(cuscode$,,9) & str(invnr$,,8)
L16860:     call "PLOWNEXT" (#10, line_key$, 17%, f1%(10))
            if f1%(10) = 0 then L17100 /* Go back through report loop */
            get #10 using L16892, seqnr$, part$, part_desc$, qty_ship,    ~
                                 unit_prc, exten, tax$, lot$, tot_cost
L16892:         FMT POS(18), CH(3), XX(3), CH(25), CH(32), XX(12),       ~
                    PD(14,4), XX(8), PD(14,4), POS(157), PD(14,4), CH(1),~
                    POS(197), CH(6), POS(630), PD(14,4)
            call "CONVERT" (qty_ship, 0.2, qty_ship$)
            call "CONVERT" (unit_prc, 2.2, unit_prc$)
            call "CONVERT" (exten,    2.2, exten$)
            call "CONVERT" (tot_cost, 2.2, tot_cost$)
            hny_key$ = all(hex(20))
            str(hny_key$,,34)  = str(part$,,25) & str(store$,,3) &       ~
                                 str(lot$,,6)
            call "READ100" (#11, hny_key$, f1%(11))
            if f1%(11) = 0 then L17020
            get #11 using L17000, tot_cost
L17000:         FMT POS(117), PD(14,4)   /* Average cost */
            call "CONVERT" (tot_cost, 2.2, tot_cost$)
L17020:     print using L18540, seqnr$, part$, part_desc$, qty_ship$,     ~
                               unit_prc$, exten$, tax$, tot_cost$
            line% = line% + 1%

            if curr$ <> "Y" or print_tran$ <> "Y" then L17080
                call "READ100" (#23, key(#10), f1%(23))
                if f1%(23) = 0% then L17080
                    get #23 using L17050, unit_cur, ext_cur
L17050:                  FMT POS(25), PD(14,4), POS(49), PD(14,4)
                    call "CURRFMT" (unit_cur, str(tran$,,4),             ~
                                                 str(unit_cur$,,10), "N")
                    str(unit_cur$,11,1) = "*"
                    call "CURRFMT" (ext_cur,  str(tran$,,4),             ~
                                                  str(ext_cur$,,10), "N")
                    str(ext_cur$,11,1) = "*"
                    print using L18550, unit_cur$, ext_cur$
                    line% = line% + 1%

L17080:     if line% > 54% then gosub report_heading
            goto L16860  /* Go after next buffer line */
L17100:     print : line% = line% + 1%  /* Print blank line */

            goto report_loop

        end_report
            if line% > 53% then gosub report_heading                     ~
                           else print using L18560
            print using L18590, rpt(6), rpt(1), rpt(2), rpt(3), rpt(4),   ~
                               rpt(5)
            if curr$ <> "Y" or print_tran$ <> "Y" then L18070
                print using L18640
L18070:     print using L18620
            call "SETPRNT" ("ARI001", " ", 0%, 1%)
            close printer
            return


        report_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L18280, date$, time$, company$
            print using L18310, postdate$, session$, descr$, page%
            print using L18340, rpt_user$, str(rpt_type$())
            print
            if print_tran$ <> "Y" then print using L18370                 ~
                                  else print using L18390
            if print_tran$ <> "Y" then print using L18420                 ~
                                  else print using L18440
            print using L18470
            return


L18280: %RUN DATE: ######## ########             ########################~
        ~######################################              UPDARIIN-AR00~
        ~1
L18310: %    POST: ########                      INVOICE ENTRY AUDIT FOR ~
        ~SESSION ###### ####################                      PAGE: ##~
        ~#
L18340: %REPORT FOR ###; TYPES ########


L18370: % SHIP-TO                                  INVOICE  INVOICE      ~
        ~INV  A/R
L18390: % SHIP-TO                                  INVOICE  INVOICE USR/ ~
        ~INV  A/R

L18420: %CUSTOMER  CUSTOMER NAME                   NUMBER    DATE   USER ~
        ~TYPE TYPE   GROSS AMT   DISCOUNT    FREIGHT  SALES TAX  NET AMOUNT
L18440: %CUSTOMER  CUSTOMER NAME                   NUMBER    DATE   CURR ~
        ~TYPE TYPE   GROSS AMT   DISCOUNT    FREIGHT  SALES TAX  NET AMOUNT

L18470: %--------- ------------------------------ -------- -------- ---- ~
        ~---- ----  ----------  ---------  ---------  ---------  ----------

L18500: %######### ############################## ######## ######## ###  ~
        ~#### #### -#######.## -######.## -######.## -######.## -#######.##

L18530: %                                                           #####~
        ~          ########### ########## ########## ########## ##########~
        ~#

L18540: %### ######################### ################################ Q~
        ~ty ########## Prc ########## Ext ########## Tax # Cost ##########

L18550: %                                                                ~
        ~                  ###########    ###########

L18560: %                                                                ~
        ~          ----------  ---------  ---------  ---------  ----------

L18590: %                               ** SESSION TOTALS (#### INVOICES)~
        ~          -#######.## -######.## -######.## -######.## -#######.##

L18620: %   *** END OF REPORT ***

L18640: %                              - Totals are in Statutory Currency~
        ~ -

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
            if str(status$,,1) = "D" or fieldnr% < 3% then L21065
                errormsg$ = "Cannot modify data when Session Status is " ~
                            & status$
                enabled%  = 0%
                return

L21065:           on fieldnr% gosub L21100,         /* Session ID       */~
                                    L21200,         /* Session Descr    */~
                                    L21300,         /* Posting Date     */~
                                    L21400          /* Audit Figures    */
                     return

L21100
*        Default/Enable for SESSION ID
            if edit% = 2% then enabled% = 0%
            inpmessage$ = "Enter Session Identifier."
            return

L21200
*        Default/Enable for SESSION DESCRIPTION
            inpmessage$ = "Enter Session Description."
            return

L21300
*        Default/Enable for POSTING DATE
            if postdate$ <> " " and postdate$ <> blankdate$ then L21310
                postdate$ = default_date$
                call "DATEFMT" (postdate$)
L21310:     inpmessage$ = "Enter G/L Posting Data for this Session."
            return

L21400
*        Default/Enable for DOCUMENT COUNT
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

                  on fieldnr% gosub L22120,         /* User ID          */~
                                    L22150,         /* Detail or Summary*/~
                                    L22170,         /* Invoice Types    */~
                                    L22220          /* Print currency?  */
                     return

L22120
*        Default/Enable for USER ID
            inpmessage$ = "Enter Specific User ID -or- 'ALL'."
            if rpt_user$ = " " then rpt_user$ = "ALL"
            return

L22150
*        Default/Enable for RPT_DET$
            inpmessage$ = "Select 'D'etail or 'S'ummary"
            if rpt_det$ = " " then rpt_det$ = "S"
            return


L22170
*        Default/Enable for INVOICE TYPES
            inpmessage$ = "Enter a non-blank character to print Type."
            if str(rpt_type$()) = " " then init("X") rpt_type$()
            return

L22220
*        Default/Enable for PRINT CURRENCY OPTION
            if curr$ <> "Y" then enabled% = 0%
            if curr$ = "Y" and print_tran$ = " " then print_tran$ = "Y"
            inpmessage$ = "Enter 'N' if only statutory values to print."
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
                str(summary$(s%),59,3) = lastuser$
                get #1 using L30220,                                      ~
                          str(summary$(s%), 5, 6),       /* Session    */~
                          str(summary$(s%),24,20),       /* Descriptn  */~
                          str(summary$(s%),14, 6),       /* Post Date  */~
                          str(summary$(s%),46, 1),       /* Status     */~
                          glpost$                        /* Post Optn  */
L30220:              FMT XX(11), CH(6), XX(3), CH(20), CH(6), CH(1),     ~
                         XX(3), CH(1)
                call "DATEFMT" (str(summary$(s%),14, 8))
                if str(summary$(s%),46,1) = "C" then                     ~
                                   str(summary$(s%),46,10) = "Closed"
                if str(summary$(s%),46,1) = "D" then                     ~
                                   str(summary$(s%),46,10) = "Data Entry"
                if str(summary$(s%),46,1) = "V" then                     ~
                                   str(summary$(s%),46,10) = "Verifying "
                if str(summary$(s%),46,1) = "U" then                     ~
                                   str(summary$(s%),46,10) = "Updating  "
                if str(summary$(s%),46,1) <> "U" then                    ~
                                   str(summary$(s%),59, 3) = " "
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
               get #1 using L30560, processor_id$, descr$, postdate$,     ~
                                   status$, lastuser$, glpost$, audit$()
L30560:              FMT CH(3), XX(17), CH(20), CH(6), CH(1), CH(3),     ~
                         CH(1), XX(27), 6*CH(10)
              /* Decipher data elements so they mean something.        */
                gosub describe_period
                if str(status$,,1)  = "C" then status$ = "Closed"
                if str(status$,,1)  = "D" then status$ = "Data Entry"
                if str(status$,,1)  = "V" then status$ = "Verifying"
                if str(status$,,1)  = "U" then status$ = "Updating"
                processor$ = "By " & processor_id$
                if str(status$,,1) <> "U" then processor$ = " "
                if str(status$,,1)  = "V" or str(status$,,1) = "U" then  ~
                                                                  return

                if month% <> 0% then return
                     errormsg$ = "Post Date invalid -- Please Correct"
                     return clear all
                     fieldnr% = 3%
                     goto L12210      /* In the middle of Edit Mode */


        describe_period   /* Describe G/L Posting Period     */
            call "WHICHPER" (#2, postdate$, glper%)
            call "WHICHMON" (#2, postdate$, month%)
            call "DATEFMT" (postdate$)
            if glpost$ = "Y" then L30820
                postdatedescr$ = "** NO G/L POSTING **"
                return
L30820:     convert glper% to postdatedescr$, pic(#0)
            postdatedescr$ = "Month (Period: " & postdatedescr$ & ")"
            on month% goto L30880, L30870, L30860
                postdatedescr$ = "INVALID " & postdatedescr$ : return
L30860:         postdatedescr$ = "Next "    & postdatedescr$ : return
L30870:         postdatedescr$ = "Current " & postdatedescr$ : return
L30880:         postdatedescr$ = "Prior "   & postdatedescr$ : return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUNFMT" (postdate$)
            plowkey$ = str(upd$) & str(session$)
            call "READ101" (#1, plowkey$, f1%(1))
            put   #1 using L31110, "   ", upd$, session$, " ", descr$,    ~
                                  postdate$, str(status$,,1), userid$,   ~
                                  glpost$, " ", audit$(), " "
L31110:         FMT CH(3), CH(8), CH(6), CH(3),                          ~
                    CH(20), CH(6), CH(1), CH(3), CH(1), CH(27),          ~
                    6*CH(10), CH(62)
            if f1%(1) = 0% then write #1  else  rewrite #1
            return

        REM *************************************************************~
            *               S U M M A R Y   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Summary Screen.                                           *~
            *************************************************************

        deffn'101
            line2$ = "  ** SUMMARY **"
            str(line2$,62) = "UPDARIIN: " & str(cms2v$,,8)
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
               at (04,06), "Session   Posting",                          ~
               at (04,59), "User",                                       ~
               at (05,06), fac(hex(ac)), hdr1$,                          ~
               at (05,15), fac(hex(ac)), hdr2$,                          ~
               at (05,25), fac(hex(ac)), hdr3$,                          ~
               at (05,47), fac(hex(ac)), hdr4$,                          ~
               at (05,59), fac(hex(ac)), hdr5$,                          ~
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
                  call "MANUAL" ("UPDARIIN")
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
            str(line2$,62) = "UPDARIIN: " & str(cms2v$,,8)
            init(hex(8c)) lfac$()
            if fieldnr% = 0% then init(hex(86)) lfac$()
            if fieldnr% = 0% then init(hex(8c)) lfac$(1)
            if edit% = 1% then gosub setpf_input else gosub setpf_edit

                  on fieldnr% gosub L41230,         /* Session ID       */~
                                    L41200,         /* Session Descr    */~
                                    L41230,         /* Posting Date     */~
                                    L41260          /* Audit Figures    */
                  goto L41300

L41200:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L41230:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L41260:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L41300:     accept                                                       ~
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
               at (06,71), fac(hex(8c))  , processor$           , ch(06),~
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
               at (11,02), "Invoice (Document) Count",                   ~
               at (11,30), fac(lfac$( 4)), audit$(1)            , ch(07),~
               at (11,42), fac(hex(84))  , actl_hdr$,                    ~
               at (11,52), fac(hex(84))  , actual$(1)           , ch(10),~
                                                                         ~
               at (12,02), "Gross Invoice Amount",                       ~
               at (12,30), fac(lfac$( 4)), audit$ (2)           , ch(10),~
               at (12,52), fac(hex(84))  , actual$(2)           , ch(10),~
                                                                         ~
               at (13,02), "Invoice Discounts",                          ~
               at (13,30), fac(lfac$( 4)), audit$ (3)           , ch(10),~
               at (13,52), fac(hex(84))  , actual$(3)           , ch(10),~
                                                                         ~
               at (14,02), "Freight",                                    ~
               at (14,30), fac(lfac$( 4)), audit$ (4)           , ch(10),~
               at (14,52), fac(hex(84))  , actual$(4)           , ch(10),~
                                                                         ~
               at (15,02), "Sales Tax",                                  ~
               at (15,30), fac(lfac$( 4)), audit$ (5)           , ch(10),~
               at (15,52), fac(hex(84))  , actual$(5)           , ch(10),~
                                                                         ~
               at (16,02), "Net Invoice Amount",                         ~
               at (16,30), fac(lfac$( 4)), audit$ (6)           , ch(10),~
               at (16,52), fac(hex(84))  , actual$(6)           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L41890
                  call "MANUAL" ("UPDARIIN")
                  goto L41300

L41890:        if keyhit% <> 15 then L41930
                  call "PRNTSCRN"
                  goto L41300

L41930:        if keyhit% <> 14 then L41990
                     plowkey$ = hex(ffffff) & str(upd$) & str(session$)  ~
                                & hex(00)
                     call "PLOWCODE" (#1, plowkey$, " ", 17%, 1.38, u3%)
                     goto L41300

L41990:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_input
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                        "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = "(4)Previous Field                       "   &        ~
                   "                       (16)Exit Program"
            str(pf$()) = hex(0001ff04ffffff0b0d0f10ffffffffffffffffff)
            if summary% = 0% and fieldnr% = 1% then return
                str(pfd$(3),64) = " " : pf$(11) = hex(ff)
                return

        setpf_edit
            if str(status$,,1) = "V" or str(status$,,1) = "U" then L42340
         pfd$(1) = "(1)Start Over         (8)Delete Session "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "(6)Close Session      (9)Audit Listing  "   &        ~
                   "                       (15)Print Screen"
         pfd$(3) = "(7)Reopen Session                       "   &        ~
                   "      (14)See Who's In (16)Save Data   "
            str(pf$()) = hex(01ffffffff06070809ff0bff0d0e0f101700ffffff)
            if fieldnr% = 0% then                                        ~
                inpmessage$ = "Position Cursor and Press Return to" &    ~
                              " Modify a displayed value."
            if str(status$,,1) <> "C" then L42300
                str(pfd$(2),  ,16) = " " : pf$(6) = hex(ff)
L42300:     if str(status$,,1) <> "D" then return
                str(pfd$(3),  ,17) = " " : pf$(7) = hex(ff)
                return

L42340:  pfd$(1) = "(1)Start Over                           "   &        ~
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
            str(line2$,62) = "UPDARIIN: " & str(cms2v$,,8)
            init(hex(8c)) lfac$()
            if fieldnr% = 0% then init(hex(86)) lfac$() else             ~
                                  init(hex(8c)) lfac$()
            if edit% = 1% then gosub setpf_rptinp else gosub setpf_rptedt

                  on fieldnr% gosub L44210,         /* User ID          */~
                                    L44210,         /* Detail or Summary*/~
                                    L44210,         /* Report Types     */~
                                    L44210          /* Print currency?  */
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
               at (10,02), "Detail or Summary?",                         ~
               at (10,36), fac(lfac$( 2)), rpt_det$             , ch(01),~
                                                                         ~
               at (11,02), "For Types: Adjustments",                     ~
               at (11,36), fac(lfac$( 3)), rpt_type$( 1)        , ch(01),~
               at (12,02), "           Credit Memos",                    ~
               at (12,36), fac(lfac$( 3)), rpt_type$( 2)        , ch(01),~
               at (13,02), "           Direct Invoices",                 ~
               at (13,36), fac(lfac$( 3)), rpt_type$( 3)        , ch(01),~
               at (14,02), "           Finance Charges",                 ~
               at (14,36), fac(lfac$( 3)), rpt_type$( 4)        , ch(01),~
               at (15,02), "           Generated Invoices",              ~
               at (15,36), fac(lfac$( 3)), rpt_type$( 5)        , ch(01),~
               at (16,02), "           Manual Invoices",                 ~
               at (16,36), fac(lfac$( 3)), rpt_type$( 6)        , ch(01),~
               at (17,02), "           On-Order Invoices",               ~
               at (17,36), fac(lfac$( 3)), rpt_type$( 7)        , ch(01),~
               at (18,02), "           Export Invoices",                 ~
               at (18,36), fac(lfac$( 3)), rpt_type$( 8)        , ch(01),~
                                                                         ~
               at (19,02), fac(hex(8c)),   print_tran_prompt$   , ch(30),~
               at (19,36), fac(lfac$( 4)), print_tran$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L44760
                  call "MANUAL" ("UPDARIIN")
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
            str(pf$()) = hex(0001ffffffffff0b0d0fffffffffffffffffffff)
            if summary% = 0% and fieldnr% = 1% then return
                str(pfd$(3),64) = " " : pf$(11) = hex(ff)
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
                  on fieldnr% gosub L51140,         /* Session ID       */~
                                    L51220,         /* Session Descr    */~
                                    L51270,         /* Posting Date     */~
                                    L51450          /* Audit Figures    */
                  return

L51140
*        Test Data for SESSION ID
            if session$ <> " " then L51170
                errormsg$ = "Session ID may not be left blank." : return
L51170:     gosub load_session
            if f1%(1) = 0% or errormsg$ <> " " then return
                return clear all
                goto edit_session

L51220
*        Test Data for SESSION DESCRIPTION
            if descr$ <> " " then return
                errormsg$ = "Session Description may not be left blank."
                return

L51270
*        Test Data for POSTING DATE
            call "DATEOK" (postdate$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (postdate$)
                gosub describe_period
                if month% > 0% then return
                     errormsg$ = "Outside of G/L Open Periods."
                     return

L51450
*        Test Data for AUDIT FIGURES
            for n% = 1% to 6%
                if audit$(n%) = " " then L51510
                     convert audit$(n%) to temp, data goto L51540
                     call "CONVERT" (temp, 2.2, audit$(n%))
                     if n% = 1% then str(audit$(1),8) = " "
L51510:     next n%
            return

L51540:     on n% goto L51550, L51560, L51570, L51580, L51590, L51600
L51550:         errormsg$ = "Invalid entry for Document Count."   : return
L51560:         errormsg$ = "Invalid entry for Gross Invoice."    : return
L51570:         errormsg$ = "Invalid entry for Invoice Discounts.": return
L51580:         errormsg$ = "Invalid entry for Freight Amount."   : return
L51590:         errormsg$ = "Invalid entry for Sales Tax."        : return
L51600:         errormsg$ = "Invalid entry for Next Invoice."     : return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Audit Report                   *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52140,         /* User ID          */~
                                    L52180,         /* Detail or Summary*/~
                                    L52220,         /* Invoice Types    */~
                                    L52270          /* Print currency?  */
                  return

L52140
*        Test Data for USER ID
            if rpt_user$ <> " " then return
                errormsg$ = "User ID may not be left blank." : return

L52180
*        Test Data for RPT_DET$
            if pos("DS" = rpt_det$) = 0 then                             ~
                errormsg$ = "Must be 'D'etail or 'S'ummary."
                return

L52220
*        Test Data for INVOICE TYPES
            if str(rpt_type$()) <> " " then return
                errormsg$ = "Please select at least one Type."
                return

L52270
*        Test Data for PRINT TRANSACTION CURRENCY?
            if enabled% = 0% then return
            if print_tran$ = "Y" or print_tran$ = "N" then return
                errormsg$ = "Please enter 'Y' or 'N'."
                return

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
