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
            *          !                                          !     *~
            *          ! 60403 CHANGES AND CUSTOMIZATIONS         !     *~
            *          !                                          !     *~
            * 12/01/90 ! UPDATE GROUP HEADER FILE AT (13240)      ! RHH *~
            * 01/01/97 ! Mods for New Planning to Update the      ! RHH *~
            *          !   Check Number. (UPDATE_GROUP)           !     *~
            * 11/25/97 ! 60403 CHANGES                            ! DJD *~
            *          ! NOTE-  NONE OF THE ARIMASTR CHANGES      !     *~
            *          ! WERE PUT IN FROM THE TEST1 VERSION       !     *~
            *          ! OF THIS PROGRAM SINCE THE T10000         !     *~
            *          ! VERSION WAS ALREADY USING THE ARIMASTR   !     *~
            *          ! FILE.  ANY REFERENCES TO FILE 20         !     *~
            *          ! IN THE TEST1 RHH VERSION ARE REMOVED     !     *~
            *          !                                          !     *~
            *          ! ALSO - THE APCPLNOR FILE IS NOW CHANNEL  !     *~
            *          ! NUMBER 52.  IT WAS 50 IN THE RHH VERSION !     *~
            *          ! BUT THAT CHANNEL WAS USED IN THE T10000  !     *~
            *          ! VERSION OF THIS PROGRAM.                 !     *~
            * 04/03/98 ! Mods to Latest Caelus Version for (EWD)  ! RHH *~
            * 05/08/98 ! (EWD001) - Mods to purge (DT) records    ! RHH *~
            *          !   when Sales Order is Paid (APCPLNDT)    !     *~
            * 04/17/01 ! Mod to print report by customer if       ! CMG *~
            *          !    settlement is not paid in full(EWD002)!     *~
            * 04/18/02 ! Mod to change company name  (EWD003)     ! TLM *~
            * 04/26/06 ! (AWD004) mod to make payment EDI file    ! CMG *~ 
	    * 05/26/06 ! (AWD005) mod to add discount to payment  ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
				         /* CHANGE 1	(EWD)          */~
            grp_cuscode$9, or_rec$170,   /* CUSTOMER NUMBER            */~
            grp_chk$8,                   /* CHECK NUMBER               */~
            grp_so$8,                    /* SALES ORDER NUMBER         */~
            grp_inv_sav$8,               /* GROUP INVOICE NUMBER       */~
            grp_inv$8,                   /* GROUP INVOICE NUMBER       */~
            grp_billto$9,                /* GROUP BILL TO NUMBER       */~
            grp_billto_sav$9,            /* GROUP BILL TO NUMBER       */~
				         /* END CHANGE 1  (EWD)        */~
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
            dt_key$23,                   /* (APCPLNDT)Prime Key(EWD001)*/~
            duedate$6,                   /* Due Date                   */~
            highardate$6,                /* High A/R Date              */~
            plowkey$50,                  /* Multi-use Plow Key         */~
            po$16,                       /* Customer's PO Number       */~
            post$1,                      /* Post to G/L?               */~
            postdate$6,                  /* G/L Posting Date           */~
            readkey$99,                                                  ~
            seq$4,                       /* Line Sequence Number       */~
            session$6,                   /* Session ID                 */~
/*AWD004*/  session_user$3,              /* User who entered session   */~
            srcecde$1,                   /* Source Code (AWD004)       */~
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

/*(EWD002) */
        dim auto_asofu$,                 /* Auto- As Of Date           */~
            bals(3), cbals(3), sbals(3), /* Balances                   */~
            rpt_date$8,                  /* Date for screen display    */~
            rpt_time$8,                  /* Report Time                */~
            title$25                     /* Report Title               */            

/* (AWD004) */
        dim date$8,                             /* Todays Date         */~
            getpadpy_key2$25,                   /* Readkey 2 GETPADPY  */~
            getpadpy_rec$(2%)256,               /* Record GETPADPY     */~
            getpadpy_seq$4,                     /* Sequence Number     */~
            time$8,                             /* Current time        */~
            operation$2,                        /* Operation           */~
            rsn_code$10                         /* Reason Code         */


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
            * #14 ! APCPLNDT ! Master Planning Detail (EWD001)          *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #42 ! CRCMSCUR ! Multi-Currency Master Information        *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #46 ! ARMTBCRC ! Multi-Currency Trial Balance Recon.      *~
            * #50 ! CCRMASTR ! Customer Credit Master file              *~
            * #51 ! ARIMASTR ! Invoice Master File                      *~
            * #52 ! APCPLNOR ! S.O. HEADER RECORD  (EWD) Mod            *~
            * #53 ! GETPADPY ! EDI FILE TO SEND PAYMENT TO DALLAS  (AWD004) *~
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
                                                   /* (EWD001) - Begin */
            select #14,  "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                                                   /* (EWD001) - End   */
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

                                              /* (EWD) - Begin  */
            select #52,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup
                                              /* (EWD) - End    */
/* (AWD004) - beg */

            select #53, "GETPADPY",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    7, keylen =   26,                    ~
                        alt key  1, keypos =    1, keylen =  32,         ~
                            key  2, keypos =    8, keylen =  25, dup
/* (AWD004) - end */   

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
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#50, fs%(50), f2%(50),   0%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))
	    call "OPENCHCK" (#52, fs%(52), f2%(52),1000%, rslt$(52))
                                               /* (EWD) - Mod   */
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

/* (AWD004)  */
            call "OPENCHCK" (#53, fs%(53), f2%(53), 500%, rslt$(53))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            call "SHOSTAT" ("UPDATING CASH RECEIPTS")
            lcnt% = 99%                       /* (EWD002)  */
            
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

*        Write Posting Headers                                 /* CRCJRLTF */
            write #11 using L10240, userid$, session$, cash_acct$,        ~
                                   postdate$, descr$, post$, " ", " ",   ~
                                   " ", " ", eod goto L10260
L10240:         FMT CH(3), CH(39), CH(9), CH(6), CH(20), 2*CH(1), CH(21),~
                    2*CH(255)                                  /* CRCRGSRF */
L10260:     write #12 using L10280, userid$, session$, cash_acct$,        ~
                                   postdate$, descr$, " ", eod goto L10300
L10280:         FMT CH(3), CH(27), CH(9), CH(6), CH(20), CH(35)

L10300:     if deposit$ = "N" then L10370                      /* CRCDEPRF */
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
/*  (AWD004)  */
                     gosub UPDATE_EDI_PAYMENT
                     goto  line_loop

            end_check
                gosub jrnl_hdr_update
                gosub regstr_hdr_update
                gosub depst_update
                gosub update_customer
                plowkey$ = str(billto$) & chk_nr$
                call "DELETE" (#09, plowkey$, 17%)
                gosub end_report
                lcnt% = 99%
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
/*AWD004*/      chk_amt, chk_disca, chk_discu, chk_gl, session_user$,     ~
                currency$
L12200:         FMT CH(9), CH(8), CH(6), CH(10), XX(6), 4*PD(14,4),      ~
                    POS(99), CH(03), POS(224), CH(4)    /*(AWD003)*/

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
/*AWD003*/                       srcedoc$, srcecde$, po$, duedate$,       ~
                                 net, disca, discu,  cracct$


L13100:         FMT POS(18), CH(4), CH(1), 2*CH(12), CH(8), CH(01),     ~
                    POS(57), CH(16), XX(8), CH(6), POS(93), 3*PD(14,4), ~
                    CH(9)              /* (AWD003) */
            gross = - (net - (disca + discu))

            call "SHOSTAT" ("UPDATING CASH RECEIPTS" & stlmnt$)
            
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
            /* CHANGE 3 (EWD) */
            gosub update_so		/* UPDATE PLNOR FILE */
L13270:
*        Last Retrieve Currency Info

            currency$ = statutory$
            tcurrency$ = statutory$
            convdate$, tconvdate$, tconvdate1$ = blankdate$
            conveqv, convunt, tconveqv, tconvunt, tconveqv1, tconvunt1 = 1
            cgross = gross:tgross = gross

            call "READ100" (#43, key(#10), f1%(43))      /* CRCLNCUR */
               if f1%(43) = 0% then L13410
            get #43 using L13400, currency$, convdate$, conveqv, convunt, ~
                                 cnet, cdisca, cdiscu
L13400:         FMT CH(4), POS(26), CH(6), 2*PD(14,7), 3*PD(14,4)
            cgross = - (cnet - (cdisca + cdiscu))
            tgross = cgross

L13410:     call "READ100" (#45, key(#10), f1%(45))      /*  CRCL2CUR */
               if f1%(45) = 0% then L13480
            get #45 using L13460, tcurrency$, tconvdate$,                 ~
                                 tconveqv, tconvunt,                     ~
                                 tconvdate1$, tconveqv1, tconvunt1,      ~
                                 tnet, tdisca, tdiscu
L13460:         FMT CH(4), POS(26), CH(6), 2*PD(14,7), CH(6), 2*PD(14,7),~
                    3*PD(14,4)
            tgross = - (tnet - (tdisca + tdiscu))

L13480:     gain_loss = 0:gain_loss$ = " "

/* (EWD002) - Mod to print is settlement is not paid in full  */
            auto_asofu$ = date
            call "ARMCBLNC" (billto$, stlmnt$, auto_asofu$, 10%,         ~
                            chk_nr$, #04, #10, sbals(), #44, #43, #45,   ~
                            currency$, convdate$, conveqv, convunt, cbals())
                            

            if currency$ <> " " then mat bals = sbals
            if currency$ =  " " then mat bals = cbals

            testbal = 0.0
            testbal = (bals(3) - (net - (disca + discu)))

            if testbal < 0.00 or testbal > 0.00                          ~
                              then gosub print_report  
/* (EWD001)  -  End  */                                        
            return

        trial_balance_update   /* Update Trial Balance with line item. */
            if pos("GS" = type$) > 0% then return

            if pos("PAD" = type$) = 0% then L14200 /* UNAPPLIED */
            if currency$ <> tcurrency$ then L14100
            if currency$ = statutory$ then L14510
                                                    /*  ARMTBCRC  */
L14100:     write #46 using L14130, currency$, billto$, stlmnt$, cgross,  ~
                      conveqv, convunt, convdate$, gross, gain_loss,     ~
                      tcurrency$, tconveqv1, tconvunt1, tconvdate1$, " "
L14130:           FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), ~
                      2*PD(14,4), CH(4), 2*PD(14,7), CH(6), CH(3)
            gain_loss = round(tgross * tconveqv1, 4%) -                  ~
                        round(tgross * tconveqv , 4%)
            total_gain_loss = total_gain_loss + gain_loss
            call "READ100" (#40, tcurrency$, f1%(40))    /*  CURMASTR  */
              if f1%(40) = 0% then L14200
            get #40 using L14160, gain_loss$
L14160:         FMT POS(41), CH(9)

L14200:     if tcurrency$ = statutory$ then L14510
                                                         /*  ARMTBCEX  */
            write #44 using L14320, tcurrency$, billto$, stlmnt$, tgross, ~
                                   tconveqv, tconvunt, tconvdate$, " "
L14320:           FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), ~
                      CH(45)
                                                         /*  ARMTRIAL  */
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
                                                        /*  CRCJRLTF   */
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
                                                           /*  CRCRGSRF  */
            write #12 using L16040, userid$, session$, billto$, chk_nr$,  ~
                                   seq$, srcedoc$, stlmnt$, disca, discu,~
                                   net, " ", eod goto L16060
L16040:         FMT CH(3), CH(6), CH(9), CH(8), CH(4), CH(8), CH(12),    ~
                    3*PD(14,4), CH(26)
L16060:     return

        regstr_hdr_update    /* Update Register with Header Info       */
                                                           /*  CRCRGSRF  */
            write #12 using L16120, userid$, session$, billto$, chk_nr$,  ~
                                   " ", billtoname$, chk_date$, chk_amt, ~
                                   " ", eod goto L16140
L16120:         FMT CH(3), CH(6), CH(9), CH(8), CH(4), CH(30), CH(6),    ~
                    PD(14,4), CH(26)
L16140:     return



        depst_update   /* Update Deposit Slip Report File              */
                                                       /*  CRCDEPRF   */
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

	/* CHANGE 4 - ADD ALL THE CODE FROM HERE TO THE END (EWD)  */

        update_group
          read #52,hold,key 4% = grp_so$, using L18175 , or_rec$,         ~
                                                         eod goto L18215
L18175:      FMT CH(170)
          if str(or_rec$,60%,2%) > "22" then goto L18215
             delete #52
          str(or_rec$,60%,2%) = "24"
          str(or_rec$,62%,8%) = date
          str(or_rec$,78%,8%) = grp_chk$
          write #52, using L18175 , or_rec$, eod goto L18215

L18215: return

        update_so                             /* 1ST FIND SALES ORDER */
            grp_billto$ = str(toss$,1%,9%)
            grp_chk$    = str(toss$,10%,8%)
            grp_inv$    = str(toss$,47%,8%)
            read #51,key 1% = grp_inv$, eod goto L18490 /* CAN HAVE DUPS*/
               goto L18390
        update_so_nxt
            read #51, eod goto L18490
L18390:        get #51, using L18410, grp_cuscode$, grp_inv_sav$, grp_so$,~
                                     grp_billto_sav$
L18410:          FMT CH(9), CH(8), POS(34), CH(8), POS(849), CH(9)
            if grp_inv$ <> grp_inv_sav$ then goto L18490
            if grp_cuscode$ = grp_billto$ then goto L18485
            if grp_billto$ = grp_billto_sav$ then goto L18485
          goto update_so_nxt
L18485: gosub update_group                        /* UPDATE (APCPLNOR) */
        gosub update_apcplndt                     /* (EWD001) - (DT)   */
L18490: return

        update_apcplndt                           /* (EWD001) - Begin  */
            init(" ") dt_key$                     /* Now Scratch S.O.  */
            str(dt_key$,1%,8%) = grp_so$          /* from Planning     */ 
        update_apcplndt_next  
            read #14,hold,key > dt_key$, using L19000, dt_key$,          ~
                                           eod goto update_apcplndt_done
L19000:        FMT POS(24), CH(23)
            if str(dt_key$,1%,8%) <> grp_so$ then                        ~
                                               goto update_apcplndt_done
               delete #14
               goto update_apcplndt_next
        update_apcplndt_done
        return                                 /* (EWD001) - End      */
	/* END CHANGE 4 (EWD) */

/* (EWD002) - Print Report if there is a net balance left over  */

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                      /* Report Header */
L55050: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-+

L55060: %!########@########                                  ############~
        ~#############                                           Page: ###~
        ~ !

L55070: %!                                   Customers with Open Net Bala~
        ~nces  Report                                                     ~
        ~ !

L55090: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-!
                                                              /* Screen*/
L55100: %!Customer !Check Num !Invoice Number!   PO Number    ! Invoice A~
        ~mt !  Allowed Disc !  Unallow Disc ! Net Paymen Amt !  Net Amt Du~
        ~e!

L55110: %!--------!-----------!--------------!----------------!----------~
        ~---!---------------!---------------!----------------!------------~
        ~-!
                                                              /* SCREEN*/
L55120: %!#########! ######## ! ############ !################!-#########~
        ~.##! -#########.## ! -#########.## !  -#########.## !-#########.#~
        ~#!
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
            
        print_report
            if str(billto$,1%,6%) = "EM0100" then return
            if str(billto$,1%,6%) = "SA0999" then return
            if str(billto$,1%,2%) = "LO" then return
            if str(billto$,1%,2%) = "RE" then return
            if str(billto$,1%,2%) = "NO" then return
            if lcnt% <> 99% then goto L55200
               gosub begin_report
L55200:     gosub print_detail
        return

        print_header                         /* GENERIC REPORT HEADING */
          if lcnt% <> 99% then print using L55050
          pageno% = pageno% + 1%
          print page
          print using L55050
          print using L55060, rpt_date$, rpt_time$, title$, pageno%
          print using L55070
          print using L55090
          print using L55100
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
          print using L55110
          print using L55120, billto$, chk_nr$, str(stlmnt$,1%,8%), po$, ~
                              bals(3%), disca, discu, net, testbal
          lcnt% = lcnt% + 2%
        return

        end_report
              if lcnt% = 99% then return
              print using L55050
              call "SETPRNT" ("CRCINP","CR01", 0%, 1%)
        return

        begin_report
              lcnt% = 99%   :  pageno% = 0%
              init(" ") rpt_date$, rpt_time$, title$
              rpt_date$ = date  :  call "DATEFMT" (rpt_date$)              
              call "TIME" (rpt_time$)            
              call "SETPRNT" ("CRCINP","CR01", 0%, 0%)
              select printer (134)
              call "COMPNAME" (12%, title$, ret%)        /* (EWD003) */
              call "SHOSTAT" ("Printing Net Balance Report ")
        return
/*  (EWD002)  - End  */

/* (AWD004) - begin */
        UPDATE_EDI_PAYMENT         /* only payments and unapplied */

            if type$ <> "P" and type$ <> "U" then return 

            if type$ = "P" then gosub PROCESS_PAYMENTS
            if type$ = "U" then gosub PROCESS_UNAPPLIED


          return

        PROCESS_PAYMENTS
            if srcecde$ <> "I" then goto PROCESS_PAYMENT_CREDIT
            init(" ") operation$
            operation$ = "00"
	    /* (AWD005) - begin */
	    getpaypy_gross = 0.00
	    getpaypy_gross = (net  - (disca + discu))
*            getpadpy_amt = -(net)
	    getpadpy_amt = -(getpaypy_gross)
	    /* (AWD005) - end */
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC
 
            if disca = 0.00 then return

            init(" ") operation$
            operation$ = "05"
            getpadpy_amt = -(disca)
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC


            return

         PROCESS_PAYMENT_CREDIT
            if srcecde$ <> "C" then return
            init(" ") operation$
            operation$ = "04"

	    /* (AWD005) - begin */
	    getpaypy_gross = 0.00
	    getpaypy_gross = (net  - (disca + discu))


*            getpadpy_amt = -(net)
	    getpadpy_amt = -(getpaypy_gross)
	    /* (AWD005) - end */
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC
 
            if disca = 0.00 then return

            init(" ") operation$
            operation$ = "05"
            getpadpy_amt = -(disca)
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC



         return

        PROCESS_UNAPPLIED
            if srcecde$ <> "C" then return
            init(" ") operation$
            operation$ = "03"


	    /* (AWD005) - begin */
	    getpaypy_gross = 0.00
	    getpaypy_gross = (net  - (disca + discu))

*            getpadpy_amt = -(net)
	    getpadpy_amt = -(getpaypy_gross)
	    /* (AWD005) - end */
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC
 
            if disca = 0.00 then return

            init(" ") operation$
            operation$ = "05"
            getpadpy_amt = -(disca)
            gosub CRT_GETPADPY_REC
            gosub WRTE_GETPADPY_REC


         return



         
        CRT_GETPADPY_REC
            init(" ") date$, getpadpy_rec$(), getpadpy_seq$, time$,      ~
                      rsn_code$
            seq% = 1%
            convert seq% to getpadpy_seq$, pic(####)
            date$ = date               
            call "DATUFMTC" (date$)
            time$ = time

           
            str(getpadpy_rec$(),1,6)   = date$       /* today */
            str(getpadpy_rec$(),7,1)   = "S"         /* send  */
            str(getpadpy_rec$(),8,9)   = billto$     /* customer */
            str(getpadpy_rec$(),17,12) = stlmnt$     /* settlement */
            str(getpadpy_rec$(),29,4)  = getpadpy_seq$  /* sequence */             
            str(getpadpy_rec$(),33,1)  = type$       /* payment type */
            str(getpadpy_rec$(),34,30) = "0"         /* bank indicator */
            str(getpadpy_rec$(),64,20) = descr$      /* Session Descr */            
            str(getpadpy_rec$(),84,10) = "USD"       /* Currency       */
            str(getpadpy_rec$(),94,20) = chk_nr$     /* check number   */

            put str(getpadpy_rec$(),114,8) using GETPADPY_FMT1,    ~
                         chk_amt
GETPADPY_FMT1:       FMT PD(14,4)

            str(getpadpy_rec$(),122,6) = postdate$    /* Posting Date   */
            str(getpadpy_rec$(),128,20) = date$ & " " & time$ & session_user$

            put str(getpadpy_rec$(),148,8) using GETPADPY_FMT1,    ~
                         getpadpy_amt
            str(getpadpy_rec$(),156,6) = date$
            str(getpadpy_rec$(),162,2) = operation$    /* operation */
            str(getpadpy_rec$(),164,10) = rsn_code$    /* Reason Code */
            str(getpadpy_rec$(),174,10) = " "          /* Jurnal Identifier*/
            str(getpadpy_rec$(),184,329) = " "         /* filler area */
                           
           
         return

        WRTE_GETPADPY_REC
            seq% = 1%
            init(" ") getpadpy_key2$
            str(getpadpy_key2$,1,21) = str(getpadpy_rec$(),8,21)

        READ_GETPADPY_REC

             read #53, hold key 2% > getpadpy_key2$, using GETPADPY_FMT2,  ~
                                    getpadpy_key2$, eod goto PUT_GETPADPY_REC

GETPADPY_FMT2:           FMT POS(8), CH(25)

                    if str(getpadpy_key2$,1,21) <> str(getpadpy_rec$(),8,21) ~
                           then goto PUT_GETPADPY_REC


                    init(" ") getpadpy_seq$
                    getpadpy_seq$ = str(getpadpy_rec$(),29,4)
                    convert getpadpy_seq$ to seq%,         ~
                                      data goto READ_GETPADPY_REC

                    seq% = seq% + 1%      /* Increment by 1 incase last one */
                    goto READ_GETPADPY_REC

PUT_GETPADPY_REC:
               convert seq% to getpadpy_seq$, pic(####)
               str(getpadpy_rec$(),29,4)  = getpadpy_seq$ /* sequence */


               write #53, using GETPADPY_FMT3, getpadpy_rec$(),   ~
                       eod goto GETPADPY_WRT_ERR

GETPADPY_FMT3:           FMT 2*CH(256)

         return

        GETPADPY_WRT_ERR

         return

/* (AWD004)  */

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
/* (EWD002) End of Report  */
            if lcnt% <> 99% then gosub end_report
            call "SHOSTAT" ("One Moment Please")
            end
            
            
