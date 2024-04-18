        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  JJJJJ  U   U  RRRR   N   N  L       *~
            *  A   A  R   R    I      J    U   U  R   R  NN  N  L       *~
            *  AAAAA  RRRR     I      J    U   U  RRRR   N N N  L       *~
            *  A   A  R   R    I    J J    U   U  R   R  N  NN  L       *~
            *  A   A  R   R  IIIII   J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIJURNL - PRINTS AN INVOICE JOURNAL BASED ON THE DATA IN *~
            *            THE 'ARIJRLTF' FILE.  POSTS TO THE GENERAL     *~
            *            LEDGER AS REQUIRED.                            *~
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
            * 09/30/86 ! Original                                 ! JIM *~
            * 04/13/87 ! Removed MIN FS Statement                 ! MJB *~
            * 11/11/87 ! Added Invoice Type 'X' for EXPORT        ! MJB *~
            * 11/27/89 ! Added Project # to text for GLDETAIL     ! JEF *~
            * 07/30/90 ! G/L export file modifications            ! RAC *~
            * 03/25/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 08/09/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* G/L acct # from ARIJRLTF   */~
            account_brk$9,               /* Controls break on acct #   */~
            account_desc$30,             /* Account description        */~
            billtoxref$9,                /* Bill to Xref from ARIJRLTF */~
            company_name$60,             /* Company name from COMPNAME */~
            cust_code$9,                 /* Customer code from ARIJRLTF*/~
            date$8,                      /* Date for screen display    */~
            desc$25,                     /* Description from ARIJRLTF  */~
            file_id$1,                   /* File ID from ARJRLTF       */~
            invoice$8,                   /* Invoice # from ARIJRLTF    */~
            invoice_type$1,              /* Invoice type from ARIJRLTF */~
            invjrnltitle$90,             /* Journal title for print    */~
            itype$(9)4,                  /* Invoice type descriptions  */~
            jrnlid$3,                    /* Journal ID (RIN)           */~
            jtitle$40,                   /* Journal title from JNLINFO */~
            module$2,                    /* Module code for GLPOST     */~
            one_time$1,                  /* Controls one-time logic    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            post_date$6,                 /* Posting date from ARIJRLTF */~
            post_desc$3,                 /* Posting description        */~
            post_flag$1,                 /* Posting flag from ARIJRLTF */~
            post_line$56,                /* Posting reference data     */~
            post_optn$14,                /* Posting option reference   */~
            post_seq$8,                  /* Edited Post sequence number*/~
            post_type$4,                 /* Posting type from ARIJRLTF */~
            project$8,                   /* Project number             */~
            prtaccount$12,               /* Edited account for printing*/~
            prtcredit$15,                /* Edited credit for printing */~
            prtdebit$15,                 /* Edited debit for printing  */~
            prtpost_date$8,              /* Edited posting date        */~
            restart$1,                   /* Restart status flag        */~
            rptid$6,                     /* Report ID                  */~
            seqnr$3,                     /* Blank or Ln # from ARIJRLTF*/~
            sesshdr_key$43,              /* Sesn header key (ARIJRLTF) */~
            session$6,                   /* Session ID from ARIJRLTF   */~
            session_desc$20,             /* Session description        */~
            sord$1,                      /* Code for ARJRLTF lookups   */~
            stor_code$3,                 /* Store code from ARIJRLTF   */~
            summary$1,                   /* Summary code from JNLINFO  */~
            summkey$1,                   /* Summary code for G/L post  */~
            text$100,                    /* Text for G/L posting       */~
            time$8,                      /* Time of day stamp          */~
            userid$3                     /* Current User Id            */

        dim                              /* G/L Posting Info           */~
            gl_post_info$(2)255          /* G/L Export Posting Info    */


        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

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
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! GLMAIN   ! GENERAL LEDGER.  SALES ACCT VERIFICATION *~
            * #3  ! ARIJRLTF ! A/R Invoicing Journal Transaction File   *~
            * #4  ! SYSFILE2 ! Caelus Management System Information     *~
            * #12 ! GLDETAIL ! GENERAL LEDGER DETIAL FILE               *~
            * #13 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #14 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #3,  "ARIJRLTF",                                      ~
                        varc,     indexed,  recsize =  638,              ~
                        keypos =    1, keylen =  43                      ~

            select #4,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #12, "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26                      
/*(AWD001)*/
            select #13, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26

            select #14, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/

REM         call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 1000%, rslt$(3 ))
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
                rslt$(12) = "REQUIRED"
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
                rslt$(13) = "REQUIRED"
/*(AWD001) - beg */
                rslt$(13) = "REQUIRED"
            call "OPENCHCK" (#13, fs%(13), f2%(13), 100%, rslt$(13))
                rslt$(14) = "REQUIRED"
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
/*(AWD001) - end */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

REM         call "SHOSTAT" ("Invoice Journal(s) are being printed")
            call "COMPNAME" (12%, company_name$, ret%)
            max_lines% = 56%
            itype$(1) = "UNKN" : itype$(2) = "ADJ "
            itype$(3) = "CM  " : itype$(4) = "DIR "
            itype$(5) = "FIN " : itype$(6) = "RCR "
            itype$(7) = "MANL" : itype$(8) = "SO  "
            itype$(9) = "EXP "
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            time$ = time
            call "TIME" (time$)
            jrnlid$ = "RIN"
            rptid$ = "ARI004"
            module$ = "01"
            session$ = all(hex(00))


                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #14, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/



        REM *************************************************************~
            *         M A I N   P R O C E S S I N G   L O O P           *~
            *************************************************************

        read_journal_header
            plowlen% = 3%
            gosub read_session_header
            if f1%(3) = 0% then goto exit_program
            on pos(" 1234"=restart$) + 1%                                ~
                goto delete_phase,       /* RESTART$ = invalid         */~
                     initialize_phase,   /* RESTART$ = blank (virgin)  */~
                     summary_phase,      /* RESTART$ = 1 (initialized) */~
                     print_phase,        /* RESTART$ = 2 (summarized)  */~
                     post_g_l_phase,     /* RESTART$ = 3 (printed)     */~
                     delete_phase        /* RESTART$ = 4 (posted)      */

        delete_phase
            plowlen% = 9%
        delete_continue
            gosub journal_tran_deleter
            if f1%(3) <> 0% then goto delete_continue
            goto read_journal_header

        initialize_phase
            ret% = 0%
            call "JNLINFO" (module$, jrnlid$, post_seq%, summary$,       ~
                jtitle$, post_date$, #4, f2%(4), ret%)
            if ret% <> 0% then goto delete_phase
            summkey$ = "N"
            post_desc$ = " "
            if post_flag$ = "N" then goto initialize_update
            if summary$ <> "Y" then goto initialize_detail
                summkey$ = "S"
                post_desc$ = "SUM"
                goto initialize_update
        initialize_detail
            summkey$ = "D"
            post_desc$ = "DTL"
        initialize_update
            restart$ = "1"
            gosub rewrite_session_header
            goto read_journal_header

        summary_phase
            sord$ = "S"
            gosub set_plowkey
            gosub journal_tran_reader
        summary_kill_summary
            if f1%(3) = 0% then goto summary_continue
            gosub journal_tran_deleter
            goto summary_kill_summary
        summary_continue
            tdebit, tcredit = 0
            sord$ = "D"
            gosub set_plowkey
        summary_summarize
            gosub journal_tran_reader
            if f1%(3) = 0% then goto summary_end_of_session
            gosub get_detail_fields
            if account$ <> account_brk$ then gosub summary_account_total
            tcredit = tcredit + credit : tdebit = tdebit + debit
            goto summary_summarize

        summary_account_total
            if tcredit + tdebit = 0 then goto summary_account_total_reset
            if tcredit > 100000000000 then tcredit = 0.00
            write #3 using L50340, userid$, session$, "S", account_brk$,  ~
                " ", account_desc$, tdebit, tcredit, userid$
        summary_account_total_reset
            gosub account_total_reset
            return

        summary_end_of_session
            gosub summary_account_total
            restart$ = "2"
            gosub rewrite_session_header
            goto read_journal_header

        print_phase
            prtpost_date$ = post_date$
            call "DATEFMT" (prtpost_date$)
            page_nbr% = 0% : nbr_lines% = 99%
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            invjrnltitle$ = jtitle$ & " JOURNAL"
            call "FMTTITLE" (invjrnltitle$, " ", 12%)
            post_optn$ = " "
            if summkey$ = "N" then post_optn$ = "NOT POSTED"
            if summkey$ = "D" then post_optn$ = "DETAIL POSTED"
            if summkey$ = "S" then post_optn$ = "SUMMARY POSTED"
            convert post_seq% to post_seq$, pic (########)
            post_line$ = "JOURNAL: " & jrnlid$ & "   " & "SEQUENCE #: " &~
                str(post_seq$,pos(post_seq$<>" ")) & "   " & post_optn$
            call "FMTTITLE" (post_line$, " ", 2%)
            sord$ = "D"
            gosub set_plowkey
            acctsw%, hdrsw%, eoprtsw% = 0%
        print_detail_loop
            gosub journal_tran_reader
            if f1%(3) = 0% then goto print_summary
            gosub get_detail_fields
            if account$ <> account_brk$ then gosub print_account_total
            tcredit = tcredit + credit : tdebit = tdebit + debit
            if nbr_lines% > max_lines% then gosub page_heading
            prtdebit$, prtcredit$ = " "
            if debit <> 0 then                                           ~
                convert debit to prtdebit$, pic (-####,###.##)
            if credit <> 0 then                                          ~
                convert credit to prtcredit$, pic (-####,###.##)
            if acctsw% = 1% then goto L14180
                prtaccount$ = account$
                call "GLFMT" (prtaccount$)
                print using L60240, prtaccount$, account_desc$, invoice$, ~
                     billtoxref$, cust_code$,                            ~
                     itype$(pos("ACDFGMOX"=invoice_type$) + 1),          ~
                     stor_code$, seqnr$, desc$, str(post_type$,,4) &     ~
                     str(prtdebit$,,12) & str(prtcredit$,,12)
                acctsw% = 1%
                goto L14230
L14180:     print using L60240, " ", " ", invoice$,                       ~
                billtoxref$, cust_code$,                                 ~
                itype$(pos("ACDFGMOX"=invoice_type$) + 1),               ~
                stor_code$, seqnr$, desc$, str(post_type$,,4) &          ~
                str(prtdebit$,,12) & str(prtcredit$,,12)
L14230:     nbr_lines% = nbr_lines% + 1%
            goto print_detail_loop

        print_account_total
            if nbr_lines% > max_lines% then gosub page_heading
            convert tdebit to prtdebit$, pic (-####,###.##)
            convert tcredit to prtcredit$, pic (-####,###.##)
            print using L60330
            print using L60360, prtaccount$, account_desc$,               ~
                str(prtdebit$,,12) & str(prtcredit$,,12)
            print skip
            nbr_lines% = nbr_lines% + 3%
            if eoprtsw% = 0% then gosub account_total_reset
            acctsw% = 0%
            return

        print_summary
            eoprtsw% = 1%
            gosub print_account_total
            print
            print using L60390            /* END OF REPORT */
            page_nbr% = 0% : nbr_lines% = 99%
            tdebit, tcredit = 0
            invjrnltitle$ = jtitle$ & " JOURNAL SUMMARY"
            call "FMTTITLE" (invjrnltitle$, " ", 12%)
            hdrsw% = 1%
            sord$ = "S"
            gosub set_plowkey
        print_summary_loop
            gosub journal_tran_reader
            if f1%(3) = 0% then goto print_summary_done
            get #3 using L50450, prtaccount$, account_desc$, debit,       ~
                credit
            tcredit = tcredit + credit : tdebit = tdebit + debit
            call "GLFMT" (prtaccount$)
            if nbr_lines% > max_lines% then gosub page_heading
            prtdebit$, prtcredit$ = " "
            if debit <> 0 then                                           ~
                convert debit to prtdebit$, pic (-###,###,###.##)
            if credit <> 0 then                                          ~
                convert credit to prtcredit$, pic (-###,###,###.##)
            print using L60270, prtaccount$, account_desc$, prtdebit$,    ~
                prtcredit$
            nbr_lines% = nbr_lines% + 1%
            goto print_summary_loop
        print_summary_done
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60290
            print using L60310, tdebit, tcredit
            print
            print using L60390
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            restart$ = "3"
            gosub rewrite_session_header
            goto  read_journal_header

        post_g_l_phase
            if summkey$ = "N" then goto post_g_l_exit
            sord$ = summkey$
            gosub set_plowkey
            one_time$ = "1"
            text$ = " " : str(text$,69) = "RECEIVABLES: JOURNAL SUMMARY"
        post_g_l_loop
            gosub journal_tran_reader
            if f1%(3) = 0% then goto post_g_l_exit
            if summkey$ = "S" then goto post_g_l_summary
                gosub get_detail_fields
                text$ = " "
                text$ = str(cust_code$,,9) & str(invoice$,,8) &          ~
                        str(seqnr$,,3) & str(project$,,8)
                if seqnr$ <> " " then goto L14960
                     str(text$,69) = desc$
                     goto post_g_l
L14960:         str(text$,31,34) = desc$
                if post_type$ = "DISC" then                              ~
                     str(text$,69) = "SALES DISCOUNT"                    ~
                else                                                     ~
                     str(text$,69) = "SALES DISTRIBUTION"
                goto post_g_l
        post_g_l_summary
            get #3 using L50450, account$, account_desc$, debit, credit

        post_g_l
            if debit <> 0 then        /*(AWD001)*/                       ~
                call "GLPOST2" (account$, debit, 0, post_date$, 0%,      ~
                            module$, text$, jrnlid$, post_seq%, userid$, ~
                            division$, #2, #12, #4, #13, ret%, invoice$,  ~
                            gl_post_info$())
            if credit <> 0 then       /*(AWD001)*/                       ~
                call "GLPOST2" (account$, 0, credit, post_date$, 0%,     ~
                            module$, text$, jrnlid$, post_seq%, userid$, ~
                            division$, #2, #12, #4, #13, ret%, invoice$, ~
                            gl_post_info$())
            if debit = 0 and credit = 0 then   /*(AWD001)*/              ~
                call "GLPOST2" (account$, 0,      0, post_date$, 0%,     ~
                            module$, text$, jrnlid$, post_seq%, userid$, ~
                            division$, #2, #12, #4, #13, ret%, invoice$, ~
                            gl_post_info$())
            goto post_g_l_loop
        post_g_l_exit
            restart$ = "4"
            gosub rewrite_session_header
            call "JNLCLOSE" (module$, jrnlid$, post_seq%, ret%)
            goto  read_journal_header

        set_plowkey
            plowlen% = 10%
            plowkey$ = str(userid$,,3) & str(session$,,6) & sord$
            str(plowkey$,11) = all(hex(00))
            one_time$ = "0"
            account$, prtaccount$ = " "
            return

        get_detail_fields
            get #3 using L50170, account$, invoice$, cust_code$,          ~
                seqnr$, post_type$, billtoxref$, invoice_type$,          ~
                stor_code$, desc$, debit, credit, project$,              ~
                gl_post_info$()
            debit = round(debit, 2)
            credit = round(credit, 2)
            if one_time$ <> "0" then return
        account_total_reset
            account_brk$ = account$
            call "DESCRIBE" (#2, account$, account_desc$, 0%, f1%(2))
            one_time$ = "1"
            tcredit, tdebit = 0
            return

        read_session_header
            plowkey$ = str(userid$,,3) & str(session$,,6)
            str(plowkey$,10) = all(hex(00))
            gosub journal_tran_reader
        read_header_loop
            if f1%(3) = 0% then return
            get #3 using L50010, session$, file_id$, post_flag$,          ~
                post_date$, session_desc$
            if file_id$ = " " then goto read_header_exit
                gosub journal_tran_deleter
                goto read_header_loop
        read_header_exit
            get #3 using L50100, restart$, post_desc$, summkey$,          ~
                post_seq%, jtitle$
            sesshdr_key$ = key(#3)    /* SAVE HEADER KEY FOR UPDATING */
            return
        journal_tran_deleter
            delete #3
        journal_tran_reader
            call "PLOWNXT1" (#3, plowkey$, plowlen%, f1%(3))
            return

        rewrite_session_header
            call "READ101" (#3, sesshdr_key$, f1%(3))
            rewrite #3 using L50100, restart$, post_desc$, summkey$,      ~
                post_seq%, jtitle$
            return

        page_heading
            page_nbr% = page_nbr% + 1%
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, prtpost_date$, invjrnltitle$, page_nbr%
            print using L60100, session$, session_desc$, post_line$
            print skip
            if hdrsw% = 0% then goto L15820
                nbr_lines% = 6%
                print using L60200
                print using L60220
                return
L15820:     nbr_lines% = 7%
            acctsw% = 0%
            print using L60120
            print using L60140
            print using L60170
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

        REM RECORD LAYOUT FOR 'ARIJRLTF' (SESSION HEADER/MAIN SECTION)
L50010:         FMT  XX(3),              /* User ID                    */~
                     CH(6),              /* Session ID                 */~
                     CH(1),              /* File ID                    */~
                     POS(44), CH(1),     /* Posting flag               */~
                     CH(6),              /* Posting date               */~
                     CH(20)              /* Session description        */

        REM RECORD LAYOUT FOR 'ARIJRLTF' (SESSION HEADER/RESTART SECTION)
L50100:         FMT  POS(71), CH(1),     /* Restart flag               */~
                     CH(3),              /* Posting option description */~
                     CH(1),              /* Summary code               */~
                     BI(4),              /* Posting sequence #         */~
                     CH(40)              /* Journal title              */

        REM RECORD LAYOUT FOR FILE 'ARIJRLTF' (DETAIL RECORD)
L50170:         FMT  XX(3),              /* User ID                    */~
                     XX(6),              /* Session ID                 */~
                     XX(1),              /* File ID                    */~
                     CH(9),              /* G/L account number         */~
                     CH(8),              /* Invoice number             */~
                     CH(9),              /* Customer code              */~
                     CH(3),              /* Sequence number            */~
                     CH(4),              /* Type designator            */~
                     CH(9),              /* Bill to Xref               */~
                     CH(1),              /* Invoice type               */~
                     CH(3),              /* Store code                 */~
                     CH(25),             /* Text/headers; Part #/lines */~
                     2*PD(14,4),         /* Debit, Credit amounts      */~
                     XX(3),              /* User ID                    */~
                     CH(8),              /* Project number             */~
                     XX(20),             /* Filler                     */~
                     2*CH(255)           /* G/L Export Posting Info    */

        REM RECORD LAYOUT FOR FILE 'ARIJRLTF' (SUMMARY RECORD)
L50340:         FMT  CH(3),              /* User ID                    */~
                     CH(6),              /* Session ID                 */~
                     CH(1),              /* File ID                    */~
                     CH(9),              /* G/L account number         */~
                     CH(24),             /* Filler                     */~
                     CH(38),             /* Account description        */~
                     2*PD(14,4),         /* Debit, Credit amounts      */~
                     CH(3),              /* User ID                    */~
                     XX(28)              /* Filler                     */

        REM RECORD LAYOUT FOR FILE 'ARIJRLTF' (SUMMARY RECORD)
L50450:         FMT  XX(3),              /* User ID                    */~
                     XX(6),              /* Session ID                 */~
                     XX(1),              /* File ID                    */~
                     CH(9),              /* G/L account number         */~
                     XX(24),             /* Filler                     */~
                     CH(38),             /* Account description        */~
                     2*PD(14,4),         /* Debit, Credit amounts      */~
                     XX(3),              /* User ID                    */~
                     XX(28),             /* Filler                     */~
                     2*CH(255)           /* GL Posting Text            */

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########        ###########################~
        ~#################################                    ARIJURNL####~
        ~###
L60070: %    POST: ########    ##########################################~
        ~################################################          PAGE: #~
        ~###
L60100: % SESSION: ###### ####################  #########################~
        ~###############################
L60120: %                                            BILL-TO   SHIP-TO   ~
        ~INV.     INV.                           POST
L60140: %G/L ACCOUNT  ACCOUNT DESCRIPTION   INVOICE  NUMBER    NUMBER    ~
        ~TYPE STR LINE DISTRIBUTION ITEM / PART  TYPE   DEBIT AMT  CREDIT ~
        ~AMT
L60170: %------------ --------------------- -------- --------- --------- ~
        ~---- --- ---- ------------------------- ---- ----------- --------~
        ~---
L60200: %                        ACCOUNT       ACCOUNT DESCRIPTION       ~
        ~         D E B I T         C R E D I T
L60220: %                        ------------  --------------------------~
        ~---   ---------------   -----------------
L60240:    %############ ##################### ######## ######### #######~
        ~## #### ###  ### ######################### ######################~
        ~######
L60270: %                        ############  ##########################~
        ~####  ###############     ###############
L60290: %                                                                ~
        ~      ---------------   -----------------
L60310: %                                               **  T O T A L S  ~
        ~**    -###,###,###.##     -###,###,###.##
L60330: %                                                                ~
        ~                                             ----------- --------~
        ~---
L60360: %                                               * ACCOUNT #######~
        ~##### ############################## TOTALS:#####################~
        ~###
L60390: %                                                        ** END O~
        ~F REPORT **

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
REM         call "SHOSTAT" ("One Moment Please")

            end
