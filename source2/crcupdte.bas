        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  C   C  R   R  C   C  U   U  P   P  D   D    T    E       *~
            *  C      RRRR   C      U   U  PPPP   D   D    T    EEEE    *~
            *  C   C  R   R  C   C  U   U  P      D   D    T    E       *~
            *   CCC   R   R   CCC    UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCUPDTE - Cash Receipts Update.  Updating is done        *~
            *            seperately by session.  User ID of operator    *~
            *            running update is used to identify this update *~
            *            from other updates running.                    *~
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
            * 12/15/86 ! Original                                 ! ERN *~
            * 12/03/87 ! Added multi-currency, CURCONVR, etc.     ! JIM *~
            * 04/17/89 ! Fixed update of Cus AR if gain or loss   ! RJM *~
            * 11/09/89 ! Statutory Currency & exchange rate table ! JDH *~
            *          !   were not being captured correctly.     !     *~
            * 12/05/89 ! Gain/Loss now calculated a bit different.! JDH *~
            * 02/09/90 ! Customer A/R balance in CUSTOMER to 4 dec! JDH *~
            * 02/15/90 ! Gain/Loss to 4 decimals, also.           ! JDH *~
            * 08/01/90 ! G/L Export file modifications.           ! JDH *~
            * 05/08/91 ! PRR 11762 Changed PLOWALTS Break Key on  ! SID *~
            *          !  UPDSESSN from 3% to 11% to avoid        !     *~
            *          !  possible ambiguity and loops.           !     *~
            * 05/23/91 ! Conditioned Execution of G/L Export code.! JBK *~
            * 11/17/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/17/92 ! Cust Credit- Upd Avg Days/Pay & # Invces ! JIM *~
            * 10/20/93 ! PRR 13031- Upd Avg Days/Pay & # Invoices ! JIM *~
            *          !   for both the Bill-To & Ship-To.        !     *~
            * 10/20/93 ! PRR 13031- Upd Last Pmt, Date & Amount   ! JIM *~
            *          !   for both the Bill-To & Ship-To.        !     *~
            * 10/20/93 ! Uses CHK_DATE$ instead of System Date    ! JIM *~
            *          !   for Date of Last Payment.              !     *~
            * 11/08/93 ! Don't update Ship-To's A/R $ in CCRMASTR.! JIM *~
            * 08/31/95 ! Corrected update of CCRMASTR.            ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$9,                      /* General Purpose Account #  */~
            applyto$12,                  /* Stlmnt # applied to        */~
            banknr$10,                   /* Check Bank Number          */~
            bill2$9, ship2$9, ccrm$9,    /* CCR Avg Days/Pay update    */~
            billto$9, billtoname$30,     /* Bill-to Code and Name      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cash_acct$9,                 /* Cash-in-Bank Account       */~
            cash_flag$1,                 /* Cash-in-Bank Account?      */~
            chk_date$6,                  /* Check Date                 */~
            chk_nr$8,                    /* Check Number               */~
            convdate$6,                  /* Currency conversion date   */~
            cracct$9,                    /* Line Item Credit Account   */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4,                  /* Currency code              */~
            deposit$1,                   /* Print Deposit Slip?        */~
            descr$20,                    /* Session Description        */~
            disca_acct$9, discu_acct$9,  /* Discount Accounts          */~
            duedate$6,                   /* Due Date                   */~
            highardate$6,                /* High A/R Date              */~
            plowkey$50,                  /* Multi-use Plow Key         */~
            po$16,                       /* Customer's PO Number       */~
            post$1,                      /* Post to G/L?               */~
            postdate$6,                  /* G/L Posting Date           */~
            readkey$99,                                                  ~
            seq$4,                       /* Line Sequence Number       */~
            session$6,                   /* Session ID                 */~
            srcedoc$8,                   /* Source Document ID         */~
            statutory$4,                 /* Statutory Currency         */~
            stlmnt$12,                   /* Line Settlement Number     */~
            toss$200,                    /* Entire Record for Toss     */~
            type$1,                      /* Line Posting Type          */~
            userid$3                     /* Current User Id            */

        dim                              /* G/L Export Posting info    */~
            country$3,                   /* Customer Country Code      */~
            custype$2,                   /* Customer type code         */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            state$2,                     /* Customer State             */~
            tran_type$5,                 /* G/L transaction type       */~
            zip$9                        /* Customer zip code          */

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #01 ! UPDSESSN ! Update Session Definitions               *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            * #04 ! ARMTRIAL ! A/R Trial Balance                        *~
            * #05 ! CRCMASTR ! Cash Receipts Check Header File          *~
            * #06 ! CRCLINES ! Cash Receipts Check Detail File          *~
            * #09 ! CRCBUFFR ! Cash Receipts Buffer- Headers            *~
            * #10 ! CRCBUF2  ! Cash Receipts Buffer- Lines              *~
            * #11 ! CRCJRLTF ! CRC Journal Transaction File             *~
            * #12 ! CRCRGSRF ! CRC Register Reporting File              *~
            * #13 ! CRCDEPRF ! CRC Deposit Slip Reporting File          *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #42 ! CRCMSCUR ! Multi-Currency Master Information        *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #46 ! ARMTBCRC ! Multi-Currency Trial Balance Recon.      *~
            * #50 ! CCRMASTR ! Customer Credit Master file              *~
            * #51 ! ARIMASTR ! Invoice Master File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    4, keylen =  17,                     ~
                        alt key  1, keypos =    1, keylen =  20

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #04, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select #05, "CRCMASTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  1,    keylen = 17

            select #06, "CRCLINES",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  1,   keylen = 21

            select #09, "CRCBUFFR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =  201, keylen =  23

            select #10, "CRCBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  21,                     ~
                        alt key  1, keypos =  201, keylen =  33

            select #11, "CRCJRLTF",                                      ~
                        varc,     indexed,  recsize =  610,              ~
                        keypos =  1,   keylen =  42

            select #12, "CRCRGSRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen = 30

            select #13, "CRCDEPRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen = 26

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

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

            select #46, "ARMTBCRC",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #50, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #51, "ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup,        ~
                            key 4, keypos = 1783, keylen = 26

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ),   0%, rslt$(1 ))
            call "OPENCHCK" (#02, fs%(2 ), f2%(2 ),   0%, rslt$(2 ))
            call "OPENCHCK" (#03, fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#04, fs%(4 ), f2%(4 ), 200%, rslt$(4 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ), 100%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ), 200%, rslt$(6 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ),   0%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 200%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 200%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 100%, rslt$(13))
            call "OPENCHCK" (#50, fs%(50), f2%(50),   0%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
            call "OPENCHCK" (#42, fs%(42), f2%(42),   0%, rslt$(42))
            call "OPENCHCK" (#43, fs%(43), f2%(43),   0%, rslt$(43))
               if fs%(42) <= 0% then L05180
                  f1%(44) = 200%:f1%(46) = 200%
L05180:     call "OPENCHCK" (#44, fs%(44), f2%(44), f1%(44), rslt$(44))
            call "OPENCHCK" (#45, fs%(45), f2%(45),   0%, rslt$(45))
               if fs%(44) <= 0% then L05200
                  f1%(46) = 200%
L05200:     call "OPENCHCK" (#46, fs%(46), f2%(46), f1%(46), rslt$(46))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            call "SHOSTAT" ("UPDATING CASH RECEIPTS")

*        Check for Multi-Currency
            curr$ = "N" : currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
               if f1%(2) = 0% then L09200
            get #02 using L09150, curr$, statutory$, currtype$
L09150:         FMT POS(21), CH(1), CH(4), POS(31), CH(1)

L09200
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#2, "SWITCHS.GL", f1%(2))
            if f1%(2) = 1% then get #2 using L09240, export_on$
L09240:         FMT POS(22), CH(1)

        REM *************************************************************~
            *           S E S S I O N   C O N T R O L                   *~
            * --------------------------------------------------------- *~
            * Get next session and write out transaction file headers.  *~
            * Then execute updating of checks and finally remove        *~
            * session from session's file.                              *~
            *************************************************************

        session_loop
*        Get next Session to Update.  If none, we're all done.
            plowkey$ = str(userid$) & "CRCUPDTE" & hex(00)
            call "PLOWALTS" (#01, plowkey$, 1%, 11%, f1%(1)) /*UPDSESSN*/
            if f1%(1) = 0% then exit_program

            get #01 using L10170, session$, descr$, postdate$, post$,     ~
                                deposit$, cash_acct$, disca_acct$,       ~
                                discu_acct$
L10170:         FMT POS(12), CH(6), XX(3), CH(20), POS(41), CH(6),       ~
                    POS(51), 2*CH(1), POS(139), 3*CH(9)

*        Write Posting Headers
            write #11 using L10240, userid$, session$, cash_acct$,        ~
                                   postdate$, descr$, post$, " ", " ",   ~
                                   " ", " ", eod goto L10260
L10240:         FMT CH(3), CH(39), CH(9), CH(6), CH(20), 2*CH(1), CH(21),~
                    2*CH(255)
L10260:     write #12 using L10280, userid$, session$, cash_acct$,        ~
                                   postdate$, descr$, " ", eod goto L10300
L10280:         FMT CH(3), CH(27), CH(9), CH(6), CH(20), CH(35)

L10300:     if deposit$ = "N" then L10370
                write #13 using L10340, userid$, session$, cash_acct$,    ~
                                       postdate$, descr$, " ",           ~
                                       eod goto L10370
L10340:              FMT CH(3), CH(23), CH(9), CH(6), CH(20), CH(39)


L10370
*        Update the Checks that are in the Session.
            gosub check_update

*        Remove Session record from the file.
            plowkey$ = "CRCUPDTE" & str(session$) & hex(00)
            call "DELETE" (#01, plowkey$, 14%)
            goto session_loop


        REM *************************************************************~
            *              U P D A T E   C H E C K S                    *~
            * --------------------------------------------------------- *~
            * Get the Checks that have been posted to session and do    *~
            * the necessary update tasks.  Toss lines as we go.         *~
            *************************************************************

        check_update

        header_loop
            gosub load_header  /* Load next header, add session data,  */
                               /* and toss into master file.           */
            if f1%(9) = 0% then return   /* Done with Session.         */

            line_loop
                gosub load_line   /* Load next line, determine Stlmnt  */
                                  /* Number, and toss into Master.     */
                if f1%(10) = 0% then end_check
                     gosub trial_balance_update
                     gosub jrnl_line_update
                     gosub regstr_line_update
                     if stlmnt$ <> "U" then gosub update_avg_days
                     plowkey$ = str(billto$) & str(chk_nr$) & seq$
                     call "DELETE" (#10, plowkey$, 21%)
                     goto  line_loop

            end_check
                gosub jrnl_hdr_update
                gosub regstr_hdr_update
                gosub depst_update
                gosub update_customer
                plowkey$ = str(billto$) & chk_nr$
                call "DELETE" (#09, plowkey$, 17%)
                goto  header_loop


        REM *************************************************************~
            *            U P D A T E   R O U T I N E S                  *~
            * --------------------------------------------------------- *~
            * The routines that do the dirty work.                      *~
            *************************************************************

        load_header  /* Loads up the next header and tosses it into    */
                     /* the Master file.                               */
            plowkey$ = str(session$) & hex(00)
            call "PLOWALTS" (#09, plowkey$, 1%, 6%, f1%(9)) /* CRCBUFFR */
            if f1%(9) = 0% then return

*        Add session level data
            put #09 using L12150, cash_acct$, disca_acct$, discu_acct$,   ~
                                postdate$, post$
L12150:         FMT POS(72), 3*CH(9), POS(108), CH(6), CH(1)

*        Now get data need for update
            get #09 using L12200, billto$, chk_nr$, chk_date$, banknr$,   ~
                chk_amt, chk_disca, chk_discu, chk_gl, currency$
L12200:         FMT CH(9), CH(8), CH(6), CH(10), XX(6), 4*PD(14,4),      ~
                    POS(224), CH(4)

            call "READ100" (#03, billto$, f1%(3))
            get #03 using L12260, billtoname$, state$, zip$, custype$,    ~
                                 country$
L12260:         FMT XX(9), CH(30), POS(208), CH(2), XX(1), CH(9),        ~
                    POS(1023), CH(2), POS(1073), CH(3)

*        Toss Header into Master File
            get #09 using L12300, toss$
L12300:         FMT CH(200)
            write #05 using L12300, toss$, eod goto L12320
            total_gain_loss = 0
L12320:     return


        load_line    /* Load the next line.  Assign settlement number  */
                     /* and toss line into master file.                */
            plowkey$ = str(billto$) & str(chk_nr$) & hex(00)
            call "PLOWNEXT" (#10, plowkey$, 17%, f1%(10))
            if f1%(10) = 0% then return

*        Get data required for update.
            get #10 using L13100, seq$, type$, stlmnt$, applyto$,         ~
                                 srcedoc$, po$, duedate$,                ~
                                 net, disca, discu,  cracct$
L13100:         FMT POS(18), CH(4), CH(1), 2*CH(12), CH(8), POS(57),     ~
                    CH(16), XX(8), CH(6), POS(93), 3*PD(14,4), CH(9)
            gross = - (net - (disca + discu))

*        Assign Settlement Number.  Rewrite buffer in case of restart.
            if stlmnt$ <> " " or pos("GS" = type$) > 0% then L13240
                if pos("PAD" = type$) > 0% then stlmnt$ = applyto$       ~
                                           else stlmnt$ = "U"
                call "ARMSTLNO" (billto$, stlmnt$, #04, #02, f1%(10))
                call "READ101" (#10, plowkey$, f1%(10))
                put #10 using L13210, stlmnt$
L13210:              FMT POS(23), CH(12)
                rewrite #10

L13240
*        Now toss the Line into the Master
            get #10 using L12300, toss$
            write #06 using L12300, toss$, eod goto L13270
L13270:
*        Last Retrieve Currency Info

            currency$ = statutory$
            tcurrency$ = statutory$
            convdate$, tconvdate$, tconvdate1$ = blankdate$
            conveqv, convunt, tconveqv, tconvunt, tconveqv1, tconvunt1 = 1
            cgross = gross:tgross = gross

            call "READ100" (#43, key(#10), f1%(43))
               if f1%(43) = 0% then L13410
            get #43 using L13400, currency$, convdate$, conveqv, convunt, ~
                                 cnet, cdisca, cdiscu
L13400:         FMT CH(4), POS(26), CH(6), 2*PD(14,7), 3*PD(14,4)
            cgross = - (cnet - (cdisca + cdiscu))
            tgross = cgross

L13410:     call "READ100" (#45, key(#10), f1%(45))
               if f1%(45) = 0% then L13480
            get #45 using L13460, tcurrency$, tconvdate$,                 ~
                                 tconveqv, tconvunt,                     ~
                                 tconvdate1$, tconveqv1, tconvunt1,      ~
                                 tnet, tdisca, tdiscu
L13460:         FMT CH(4), POS(26), CH(6), 2*PD(14,7), CH(6), 2*PD(14,7),~
                    3*PD(14,4)
            tgross = - (tnet - (tdisca + tdiscu))

L13480:     gain_loss = 0:gain_loss$ = " "
            return

        trial_balance_update   /* Update Trial Balance with line item. */
            if pos("GS" = type$) > 0% then return

            if pos("PAD" = type$) = 0% then L14200 /* UNAPPLIED */
            if currency$ <> tcurrency$ then L14100
            if currency$ = statutory$ then L14510

L14100:     write #46 using L14130, currency$, billto$, stlmnt$, cgross,  ~
                      conveqv, convunt, convdate$, gross, gain_loss,     ~
                      tcurrency$, tconveqv1, tconvunt1, tconvdate1$, " "
L14130:           FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), ~
                      2*PD(14,4), CH(4), 2*PD(14,7), CH(6), CH(3)
            gain_loss = round(tgross * tconveqv1, 4%) -                  ~
                        round(tgross * tconveqv , 4%)
            total_gain_loss = total_gain_loss + gain_loss
            call "READ100" (#40, tcurrency$, f1%(40))
              if f1%(40) = 0% then L14200
            get #40 using L14160, gain_loss$
L14160:         FMT POS(41), CH(9)

L14200:     if tcurrency$ = statutory$ then L14510

            write #44 using L14320, tcurrency$, billto$, stlmnt$, tgross, ~
                                   tconveqv, tconvunt, tconvdate$, " "
L14320:           FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), ~
                      CH(45)

L14510:         write #04 using L14600, billto$, stlmnt$,                 ~
                                      0, duedate$, 0%, duedate$, po$,    ~
                                      " ", " ", gross - gain_loss,       ~
                                      cracct$, "01", "C", type$,         ~
                                      chk_nr$, postdate$, chk_date$, " ",~
                                      " ", " ", " ",                     ~
                                      0, duedate$, 0%, duedate$, po$,    ~
                                      currency$, convdate$, conveqv,     ~
                                      convunt, " ", eod goto L14650
L14600:              FMT CH(9), CH(12), PD(14,4), CH(6), BI(1), CH(6),   ~
                         CH(16), CH(3), CH(6), PD(14,4), CH(9), CH(2),   ~
                         2*CH(1), CH(8), 2*CH(6), CH(9), CH(16), 2*CH(3),~
                         PD(14,4), CH(6), BI(1), CH(6), CH(16), CH(4),   ~
                         CH(6), 2*PD(14,7), CH(54)
L14650:         return


        jrnl_line_update   /* Update Journal Trans File with line item */
            cash_flag$ = "N"
            dr = 0  :  cr = -(gross - gain_loss) : acct$ = cracct$
            tran_type$ = "RCR01"
            gosub jrnl_update
            temp$ = type$ : type$ = "X"
            dr = 0  :  cr = -gain_loss : acct$ = gain_loss$
            tran_type$ = "RCR02"
            gosub jrnl_update
            type$ = temp$
            return

        jrnl_hdr_update    /* Update Journal Trans File with Hdr Accts */
            seq$ = hex(00000000)  :  type$, stlmnt$ = " "
            cash_flag$ = " "
            dr = chk_amt       :  cr = 0    :  acct$ = cash_acct$
            tran_type$ = "RCR03"
            gosub jrnl_update

            seq$ = hex(00000001)  :  cash_flag$ = "N"
            dr = -chk_disca    :  cr = 0    :  acct$ = disca_acct$
            tran_type$ = "RCR04"
            gosub jrnl_update

            seq$ = hex(00000002)
            dr = -chk_discu    :  cr = 0    :  acct$ = discu_acct$
            tran_type$ = "RCR05"
            gosub jrnl_update  :  seq$ = " "
            return

            jrnl_update   /* Common rtn for writing to file  */
              if dr = 0 and cr = 0 then return
                if export_on$ = "Y" then gosub load_gl_info
                if dr >= 0 then L15300  :   cr = -dr  :  dr = 0
L15300:         if cr >= 0 then L15310  :   dr = -cr  :  cr = 0
L15310:         write #11 using L15360, userid$, session$, "D",           ~
                                       cash_flag$, acct$, billto$,       ~
                                       chk_nr$, seq$, type$, stlmnt$,    ~
                                       dr, cr, " ", gl_post_info$(),     ~
                                       eod goto L15390
L15360:              FMT CH(3), CH(6), CH(1), CH(1), CH(9), CH(9), CH(8),~
                         CH(4), CH(1), CH(12), 2*PD(14,4), CH(30),       ~
                         2*CH(255)
L15390:         return



        load_gl_info

            put str(gl_post_info$(),,) using L15660,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
                convunt,                 /* Currency Units per Book    */~
                (dr-cr),                 /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                billto$,                 /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                custype$,                /* Customer Type CH(2)        */~
                state$,                  /* State CH(2)                */~
                country$,                /* Country CH(3)              */~
                zip$,                    /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                " ",                     /* Part Number CH(25)         */~
                " ",                     /* Part Category CH(4)        */~
                " ",                     /* Part Class CH(4)           */~
                " ",                     /* Part Generic code CH(16)   */~
                " ",                     /* Part Type CH(3)            */~
                " ",                     /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                chk_nr$,                 /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L15660: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice CH(16)      */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

        regstr_line_update   /* Update Register with Line Item         */
            write #12 using L16040, userid$, session$, billto$, chk_nr$,  ~
                                   seq$, srcedoc$, stlmnt$, disca, discu,~
                                   net, " ", eod goto L16060
L16040:         FMT CH(3), CH(6), CH(9), CH(8), CH(4), CH(8), CH(12),    ~
                    3*PD(14,4), CH(26)
L16060:     return

        regstr_hdr_update    /* Update Register with Header Info       */
            write #12 using L16120, userid$, session$, billto$, chk_nr$,  ~
                                   " ", billtoname$, chk_date$, chk_amt, ~
                                   " ", eod goto L16140
L16120:         FMT CH(3), CH(6), CH(9), CH(8), CH(4), CH(30), CH(6),    ~
                    PD(14,4), CH(26)
L16140:     return



        depst_update   /* Update Deposit Slip Report File              */
            if deposit$ <> "Y" then return
                write #13 using L17060, userid$, session$, billto$,       ~
                                       chk_nr$, billtoname$, chk_date$,  ~
                                       banknr$, chk_amt, " ",            ~
                                       eod goto L17080
L17060:              FMT CH(3), CH(6), CH(9), CH(8), CH(30), CH(6),      ~
                         CH(10), PD(14,4), CH(20)
L17080:         return



        update_customer
*        Update CCRMASTR with latest payment data, A/R Balance, etc.
*        Fields are updated in CCRMASTR, the CUSTOMER shadow file.
            gosub'201(billto$, 0%)/* Update Bill-To Pmt $, Date & Chk # */
                return

        deffn'201(ccrm$, u%)
            highardate$ = " " : ar, highar = 0                  /* JIC */
            call "READ101" (#50, ccrm$, f1%(50%))          /* CCRMASTR */
            if f1%(50%) <> 0% then get #50 using L18070, highardate$, ar, ~
                highar             /* Get CCRMASTR fields for updating */
L18070:         FMT POS(108), CH(6), POS(130), 2*PD(14,4)
            if f1%(50%) = 0% then put #50 using L18530, billto$, 0, 0, 0, ~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "   /* Create new CCRMASTR record */
            if u% <> 0% then goto L18160  /* Don't update Ship-To's A/R */
                ar = ar - (chk_amt - (chk_disca + chk_discu) - chk_gl)
                ar = round(ar - total_gain_loss, 4)
                if ar < highar then L18160
                     highar      = ar
                     highardate$ = date
L18160:     put #50 using L18180, chk_amt, chk_nr$, date, chk_date$,      ~
                highardate$, ar, highar, userid$, date     /* CCRMASTR */
L18180:         FMT POS(56), PD(14,4), CH(10), POS(84), CH(6), POS(96),  ~
                     CH(6), POS(108), CH(6), POS(130), 2*PD(14,4),       ~
                     POS(146), CH(3), CH(6)
            if f1%(50%) = 0% then write #50 else rewrite #50 /*CCRMASTR*/
            return

        update_avg_days
*        Update Avg # Days to Pay, # Invoices in CCRMASTR.
            readkey$ = str(billto$) & str(stlmnt$,,10%) & "00"
            call "READ100" (#04, readkey$, f1%(4%))        /* ARMTRIAL */
                if f1%(4%) = 0% then return /* No invoice in Trial Bal */
            get #04 using L18264, bill2$, ship2$            /* ARMTRIAL */
L18264:         FMT CH(9), POS(109), CH(9)
            readkey$ = str(ship2$) & srcedoc$
            call "READ100" (#51, readkey$, f1%(51%))       /* ARIMASTR */
                if f1%(51%) = 0% then return             /* No invoice */
            get #51 using L18300, invdate$     /* Original Invoice Date */
L18300:         FMT POS(521), CH(6)
            call "DATE" addr ("G-", invdate$, chk_date$, days%, u3%)
            if u3% <> 0% then return                /* Invalid date(s) */
            gosub'200(bill2$)           /* Update Bill-To Avg Days/Pay */
            if ship2$ = bill2$ or ship2$ = " " then return
                gosub'200(ship2$)       /* Update Ship-To Avg Days/Pay */
                gosub'201(ship2$, 1%)   /* Update Ship-To Payment data */
                return

        deffn'200(ccrm$)
            avgdays, nbrinvcs% = 0                              /* JIC */
            call "READ101" (#50, ccrm$, f1%(50%))          /* CCRMASTR */
            if f1%(50%) <> 0%                                            ~
                then get #50 using L18480, avgdays, nbrinvcs%             ~
                else put #50 using L18530, billto$, 0, 0, 0, " ", 0,      ~
                " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0, 0,       ~
                0, 0, " ", " ", " "      /* Create new CCRMASTR record */
            avgdays = avgdays * nbrinvcs%     /* De-compute raw # days */
            nbrinvcs% = nbrinvcs% + 1%    /* Bump # of Invoices in Avg */
            avgdays = avgdays + days%       /* Bump raw number of days */
            if nbrinvcs% = 0% then return     /* Divisor can't be zero */
*        No anomaly- Compute the new Average Number of Days to Pay.
                avgdays = round(avgdays / nbrinvcs%, 4)
                put #50 using L18480, avgdays, nbrinvcs%, userid$, date
L18480:              FMT POS(74), PD(14,4), BI(2), POS(146), CH(3), CH(6)
                if f1%(50%) = 0% then write #50 else rewrite #50
                return

L18530:     FMT /* File #50- CCRMASTR Master file                      */~
                CH(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Invoices in Average Days */~
                5*CH(6),       /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),    /* 114/32-   'Dynamic' amnts fr CUSTOMER*/~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

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
            end
