        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   JJJJJ  U   U  RRRR   N   N  L       *~
            *  C   C  R   R  C   C    J    U   U  R   R  NN  N  L       *~
            *  C      RRRR   C        J    U   U  RRRR   N N N  L       *~
            *  C   C  R   R  C   C  J J    U   U  R  R   N  NN  L       *~
            *   CCC   R   R   CCC    J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCJURNL - Prints Cash Receipts Journal and Posts G/L.    *~
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
            * 04/28/87 ! Fixed problem regarding cash-in-bank     ! ERN *~
            *          !   not posting when accounts packed.      !     *~
            * 02/09/90 ! Rounded DR & CR amounts before printing. ! JDH *~
            * 08/02/90 ! G/L export file modifications.           ! RAC *~
            * 03/08/93 ! PRR 11206 Fixed chnl #10 test after chnl ! JIM *~
            *          !   #1 READ.                               !     *~
            * 03/09/93 ! PRR 12535 Added 'G/L Not Posted' message.! JIM *~
            * 05/04/06 ! (AWD001) mod for GET PAID Cash Data      ! CMG *~
            * 08/14/06 ! (AWD002) Mod to add GLORTRAN             ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12, acctdescr$30,       /* Account and Description    */~
            billto$9,                    /* Bill-to Customer Code      */~
            cashacct$9,                  /* Session Cash-in-Bank Acct  */~
            check$8,                     /* Check Number               */~
            compname$60,                 /* Company Name               */~
            cr$14,                       /* CR Amount- Output          */~
            ctl$10,                      /* Report Break Control       */~
            date$8,                      /* Run Date                   */~
            dr$14,                       /* DR Amount- Output          */~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_post_msg$14,              /* G/L Post message           */~
            jrnl_title$60,               /* Journal Title              */~
            plowkey$50,                  /* Plow Key                   */~
            postdate$8,                  /* Session Post Date          */~
            postflag$1,                  /* Posting Summary Level      */~
            postseq$8,                   /* Posting Sequence Number    */~
            readkey$50,                  /* A Read Key                 */~
            runtime$8,                   /* Report Run Time            */~
            session$6,                   /* Session ID                 */~
            session_post$1,              /* Session- Post G/L?         */~
            session_status$,             /* Status Flag- ' ',S,R,U,C   */~
            stlmnt$14,                   /* Settlement Number          */~
            text$100,                    /* G/L Posting Text           */~
            type$17,                     /* Payment Type               */~
            userid$3                     /* Current User Id            */~

/* (AWD001)  */
        dim getpadpy_key$26,             /* GETPADPY Readkey           */~
            getpadpy_rec$(2%)256         /* GETPADPY Record            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        dim division$3,                  /* division number (AWD002)   */~
            schema$8                     /* schema          (AWD002)   */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #1  ! CRCJRLTF ! Cash Receipts Register Report File       *~
            * #2  ! SYSFILE2 ! CMS System File                          *~
            * #3  ! GLMAIN   ! G/L Chart of Accounts                    *~
            * #4  ! GLDETAIL ! G/L Detail                               *~
            * #5  ! GETPADPY ! EDI FILE TO SEND PAYMENT TO DALLAS  (AWD001) *~
            * #14 ! GLORTRAN ! GL Oracle transmit file     (AWD002)     *~
            * #15 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD002)     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CRCJRLTF",                                      ~
                        varc,     indexed,  recsize =  610,              ~
                        keypos =    1, keylen =  42

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  9

            select #4,  "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26

/* (AWD001) - beg */

            select #5,  "GETPADPY",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    7, keylen =   26,                    ~
                        alt key  1, keypos =    1, keylen =  32,         ~
                            key  2, keypos =    8, keylen =  25, dup
/* (AWD001) - end */ 

/* (AWD002) */
            select #14, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 17,                       ~
                        alt key 1, keypos = 15, keylen = 31

            select #15, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD002)  */

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
/*(AWD001)*/
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))

/*(AWD002)*/
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
/*(AWD002)*/
/*(AWD002)*/
           if f2%(14) = 0 then L09000
           call "OPENFILE" (#14, "OUTPT", f2%(14), rslt$(14), axd$(14))
           close #14
           call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Printing/Posting Cash Receipts Journal")
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (12%, compname$, f1%(2))
            call "SETPRNT" ("CRC004", " ", 0%, 0%)
            select printer (134)



                                                          /* (AWD002)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #15, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


        REM *************************************************************~
            *                P R I N T    &   P O S T                   *~
            *-----------------------------------------------------------*~
            * Print a report and post G/L for each session on file.     *~
            *************************************************************


        session_loop
            plowkey$ = str(userid$) & hex(00)
            call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1))
            if f1%(1) = 0% then exit_program

L10120:     get #1 using L10140, session$, cashacct$, postdate$,          ~
                                session_post$, session_status$,          ~
                                postseq$, postflag$
L10140:         FMT POS(4), CH(6), POS(43), CH(9), CH(6), POS(78),       ~
                    2*CH(1), CH(8), CH(1)


            if postflag$ = "N"                                           ~
                then gl_post_msg$ = "G/L NOT POSTED"                     ~
                else gl_post_msg$ = " "
            on pos(" SRUC" = session_status$) gosub start_up, summarize, ~
                                                    print_report, post,  ~
                                                    clean_up
            goto L10120      /* Re-cycle the session header record data */


        start_up          /* 1st time for session- get posting info    */
            ret% = 0%
            call "JNLINFO" ("01", "RCR", postseq%, postflag$,            ~
                             jrnl_title$, postdate$, #2, f2%(2), ret%)
            convert postseq% to postseq$, pic(########)
            on pos("FNDY" = postflag$) goto L10320, L10300, L10320, L10310
                postflag$ = "D"  :  goto L10320     /* Unknown */
L10300:         postflag$ = "D"  :  goto L10320     /* 'N'     */
L10310:         postflag$ = "S"                    /* 'Y'     */
L10320:     if session_post$ = "N" then postflag$ = "N"
            session_status$  = "S"
            gosub update_session_header
            return


        clean_up     /* Delete any records left for this session       */
            plowkey$ = str(userid$) & str(session$) & hex(00)
            call "DELETE" (#1, plowkey$, 9%)
            return clear
            call "JNLCLOSE" ("01", "RCR", postseq%, ret%)
            goto session_loop


        update_session_header  /* Rewrite Session Header w/ new info   */
            readkey$ = str(userid$) & str(session$)
            call "READ101" (#1, readkey$, f1%(1%))         /* CRCJRLTF */
            put #1 using L10490, session_status$, postseq$, postflag$
L10490:         FMT POS(79), CH(1), CH(8), CH(1)
            rewrite #1
            return


        REM *************************************************************~
            *                  S U M M A R I Z E                        *~
            *-----------------------------------------------------------*~
            * Create summary by account.                                *~
            *************************************************************
        summarize

*        First kill any summary records that may exist for this session.
            plowkey$ = str(userid$) & str(session$) & "S"
            call "DELETE" (#1, plowkey$, 10%)
            plowkey$ = str(userid$) & str(session$) & "T"
            call "DELETE" (#1, plowkey$, 10%)

*        Now summarize transactions.
            plowkey$ = all(hex(00))
            str(plowkey$,,10) = str(userid$) & str(session$) & "D"
            cashdr, cashcr = 0

        summarize_loop
            call "PLOWNEXT" (#1, plowkey$, 10%, f1%(1))
            if f1%(1) = 0% then end_summarization
                get #1 using L11210, acct$, dr, cr
L11210:              FMT POS(11), CH(9), POS(55), 2*PD(14,4)
                dr = round(dr, 2)
                cr = round(cr, 2)
                readkey$ = str(plowkey$,,20) : str(readkey$,10,1) = "S"
                gosub update_summary
                if str(plowkey$,11,1) <> " " then summarize_loop
                     cashdr = cashdr + dr
                     cashcr = cashcr + cr
                     goto summarize_loop


        end_summarization
            readkey$ = str(plowkey$,,9) & "T " & cashacct$
            dr = cashdr
            cr = cashcr
            gosub update_summary
            session_status$ = "R"
            gosub update_session_header
            return


        update_summary
            sdr, scr = 0
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then get #1 using L11440, sdr, scr
L11440:         FMT POS(55), 2*PD(14,4)
            sdr = round(sdr, 2)
            scr = round(scr, 2)
            sdr = sdr + dr
            scr = scr + cr
            put #1 using L11480, readkey$, " ", sdr, scr, " "
L11480:         FMT CH(41), CH(13), 2*PD(14,4), CH(30)
            if f1%(1) = 0% then write #1  else  rewrite #1
            return


        REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            *-----------------------------------------------------------*~
            * Print Detail and Summary Journals for the Session.        *~
            *************************************************************
        print_report
            adr, acr, sdr, scr = 0
            page%, accts% = 0%
            report% = 1%
            ctl$    = hex(00)
            plowkey$ = all(hex(00))
            str(plowkey$,,10) = str(userid$) & str(session$) & "D"
            ret% = 2%
            call "JNLINFO" ("01", "RCR", temp%, temp$, jrnl_title$,      ~
                  postdate$, #2, f2%(2), ret%) : temp$ = " " : temp% = 0%
            call "FMTTITLE" (jrnl_title$, "JOURNAL", 12%)
            call "DATEFMT"  (postdate$)
            gosub page_heading


        detail_loop
            call "PLOWNEXT" (#1, plowkey$, 10%, f1%(1))
            if f1%(1) = 1% then L12260
                gosub account_totals
                print using L13610, sdr, scr
                goto  summary_report

L12260:     if ctl$ = hex(00) then L12290
            if ctl$ = str(plowkey$,11,10) then L12360
                gosub account_totals
L12290:         ctl$  = str(plowkey$,11,10)
                acct$ = str(plowkey$,12, 9)
                call "DESCRIBE" (#3, acct$, acctdescr$, 0%, f1%(3))
                call "GLFMT" (acct$)
                accts%     = accts% + 1%
                acctprint% = 1%

L12360:     get #1 using L12370, billto$, check$, type$, stlmnt$, dr, cr
L12370:         FMT POS(21), CH(9), CH(8), POS(42), CH(1), CH(12),       ~
                                                               2*PD(14,4)
            dr$, cr$ = " "
            dr = round(dr, 2)
            if dr <> 0 then convert dr to dr$, pic(-##,###,###.00)
            cr = round(cr, 2)
            if cr <> 0 then convert cr to cr$, pic(-##,###,###.00)
            if type$ = " " then type% = val(str(plowkey$,41,1))
            if type$ = " " then convert type% to type$, pic(0)
            if type$ = "0" then type$ = "CASH RECEIVED"
            if type$ = "1" then type$ = "ALLOWED DISCS"
            if type$ = "2" then type$ = "UNALLWD DISCS"
            if type$ = "P" then type$ = "APPLIED PYMNT"
            if type$ = "A" then type$ = "ADJUSTMENT   "
            if type$ = "B" then type$ = "BAL FWD PYMNT"
            if type$ = "U" then type$ = "UNAPPLIED    "
            if type$ = "G" then type$ = "G/L DISTR.   "
            if type$ = "S" then type$ = "DIRECT SALE  "
            if type$ = "D" then type$ = "BAL FWD DISTR"
            if type$ = "X" then type$ = "EXCHANGE G/L "
            if stlmnt$ <> " " then stlmnt$ = str(stlmnt$,1,8) & "-" &    ~
                                             str(stlmnt$,9,2) & "-" &    ~
                                             str(stlmnt$,11)
            if line% > 54% then gosub page_heading
            if acctprint% > 0% then                                      ~
                print using L13520, acct$, acctdescr$, billto$, check$,   ~
                                   type$, stlmnt$, dr$, cr$              ~
                     else                                                ~
                print using L13520, " ",   " ",        billto$, check$,   ~
                                   type$, stlmnt$, dr$, cr$
            acctprint% = 0%
            line%  = line%  + 1%
            adr    = adr    + dr
            acr    = acr    + cr

            goto detail_loop


            account_totals
                if accts% = 0% then return
                     print using L13550
                     print using L13580, acct$, adr, acr
                     print
                     line%    = line%   + 3%
                     sdr      = sdr     + adr
                     scr      = scr     + acr
                     adr, acr = 0
                     return



        summary_report
            if accts% = 0% then end_report_print
            plowkey$ = all(hex(00))
            str(plowkey$,,10) = str(userid$) & str(session$) & "S"
            report%  = 2%
            gosub page_heading

        summary_report_loop
            call "PLOWNEXT" (#1, plowkey$, 10%, f1%(1%))
            if f1%(1%) = 1% then L12980
                print using L13750
                print using L13780, sdr, scr
                print : print "** END OF REPORT **"
                goto end_report_print

L12980:     get #1 using L12990, acct$, dr, cr
L12990:         FMT POS(12), CH(9), POS(55), 2*PD(14,4)
            dr = round(dr, 2)
            cr = round(cr, 2)
            call "DESCRIBE" (#3, acct$, acctdescr$, 0%, f1%(3))
            call "GLFMT" (acct$)
            if line% > 54% then gosub page_heading
            print using L13720, acct$, acctdescr$, dr, cr
            line% = line% + 1%
            goto summary_report_loop


            page_heading
                page% = page% + 1%
                line% = 7%
                print page
                print using L13340, date$, runtime$, compname$
                print using L13370, postdate$, session$, gl_post_msg$,    ~
                     jrnl_title$, page%
                print using L13400, postseq$
                if report% = 2% then L13210
                     print using L13430
                     print using L13460
                     print using L13490
                     return

L13210:              print
                     print using L13660
                     print using L13690
                     return


        end_report_print
            session_status$ = "U"
            gosub update_session_header
            return



L13340: %RUN DATE: ######## ########                 ####################~
        ~########################################             CRCJURNL-CRC~
        ~004
L13370: %POST: ######## (######) ##############      ####################~
        ~########################################                 PAGE: ##~
        ~##
L13400: %POST SEQ: ########


L13430: %                                                  BILL-TO   CHEC~
        ~K                   PAYMENT

L13460: %   G/L ACCOUNT  ACCOUNT DESCRIPTION               NUMBER    NUMB~
        ~ER   PAYMENT TYPE   SETTLEMENT NO.    DEBIT AMOUNT   CREDIT AMOUN~
        ~T
L13490: %   ------------ ------------------------------  ---------  -----~
        ~---  -------------  --------------   -------------   ------------~
        ~-
L13520: %   ############ ##############################  #########  #####~
        ~###  #############  ##############  ##############  #############~
        ~#
L13550: %                                                                ~
        ~                                     -------------   ------------~
        ~-
L13580: %                                                                ~
        ~ * TOTAL ACCOUNT ############       -##,###,###.##  -##,###,###.#~
        ~#
L13610: %                                                                ~
        ~** J O U R N A L   T O T A L S      -##,###,###.##  -##,###,###.#~
        ~#


L13660: %                             ACCOUNT     ACCOUNT DESCRIPTION    ~
        ~            D E B I T S     C R E D I T S

L13690: %                           ------------  -----------------------~
        ~-------   --------------   --------------

L13720: %                           ############  #######################~
        ~#######  -###,###,###.##  -###,###,###.##

L13750: %                                                                ~
        ~          --------------   --------------

L13780: %                                            **  T O T A L S  ** ~
        ~         -###,###,###.##  -###,###,###.##

        REM *************************************************************~
            *                    P O S T   G / L                        *~
            *-----------------------------------------------------------*~
            * Post transactions to G/L.                                 *~
            *************************************************************
        post
            if postflag$ = "N" then end_postings

            plowkey$ = all(hex(00))
            str(plowkey$,,10) = str(userid$) &  str(session$)  &  "D"
            if postflag$ = "S" then str(plowkey$,10,1) = "S"

        post_loop
            call "PLOWNEXT" (#1, plowkey$, 10%, f1%(1))
            if f1%(1) = 0% then end_postings

            if postflag$ = "D" and str(plowkey$,11,1) = " " then post_loop

            get #1 using L14180, acct$, billto$, check$, type$, stlmnt$, ~
                                dr, cr,              /* (AWD001)  */ ~
                                gl_post_info$()

                                                           /* (AWD001) */
L14180:         FMT POS(12), 2*CH(9), CH(8), POS(42), CH(1), CH(12), ~
                    POS(55), 2*PD(14,4), POS(101), 2*CH(255)
            dr = round(dr, 2)
            cr = round(cr, 2)
            net = dr - cr
            dr, cr = 0
            if net > 0 then  dr = net  else  cr = -net
            if net = 0 then  post_loop

            if postflag$ <> "S" then L14290
                text$ = " "
                str(text$,69) = "Summary Cash Receipts Postings"
                goto L14440
L14290:     text$ = str(billto$) & check$
            if type$ = " " then type% = val(str(plowkey$,41,1))
            if type$ = " " then convert type% to type$, pic(0)
            if type$ = "0" then type$ = "Cash-in-Bank     "
            if type$ = "1" then type$ = "Allowed Discounts"
            if type$ = "2" then type$ = "Unallowed Discs  "
            if type$ = "P" then type$ = "Applied Payment  "
            if type$ = "A" then type$ = "Adjustment       "
            if type$ = "B" then type$ = "Bal Fwd Payment  "
            if type$ = "U" then type$ = "Unapplied Payment"
            if type$ = "G" then type$ = "G/L Distribution "
            if type$ = "S" then type$ = "Direct Sale      "
            if type$ = "D" then type$ = "Bal Fwd Distr.   "
            if type$ = "X" then type$ = "Exchnge Gain/Loss"
            str(text$,69) = "Cash Receipts: " & type$

L14440:     gosub glpost

            gosub UPDATE_GETPADPY                        /* (AWD001)  */

            goto  post_loop


        end_postings      /* Post Cash-in-Bank Summary and Be Gone     */
            if postflag$ <> "D" then L14650
                readkey$ = str(userid$) & str(session$) & "T " &         ~
                           cashacct$
                call "READ101" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then L14650
                     get #1 using L14550, acct$, dr, cr
L14550:                   FMT POS(12), CH(9), POS(55), 2*PD(14,4)
                     dr = round(dr, 2)
                     cr = round(cr, 2)
                     net = dr - cr
                     dr, cr = 0
                     if net > 0 then  dr = net  else  cr = -net
                     if net = 0 then L14650
                          text$ = " "
                          str(text$,69) =                                ~
                                       "Cash Receipts: Cash-in-Bank Smry"
                          gosub glpost

L14650:     session_status$ = "C"
            gosub update_session_header
            return


            glpost
                call "GLPOST2" (acct$, dr, cr, postdate$, 0%, "01",      ~
                               text$, "RCR", postseq%, userid$, division$, ~
                               #3,#4,#2, #14,     /* (AWD002)  */          ~
                               ret%, check$, gl_post_info$())
                return



        UPDATE_GETPADPY
            init(" ") getpadpy_key$, getpadpy_rec$()

            str(getpadpy_key$,1,1)   = "S"
            str(getpadpy_key$,2,9)   = billto$
            str(getpadpy_key$,11,12) = stlmnt$

        GETPADPY_NEXT
            read #5, hold, key > getpadpy_key$, using GETPADPY_FMT1,    ~
                                      getpadpy_rec$(), eod goto GETPADPY_DONE

GETPADPY_FMT1:     FMT 2*CH(256)


* if not (s)end, billto and stlment then finished
                    if str(getpadpy_key$,1,22) <> str(getpadpy_rec$(),7,22) ~
                           then goto GETPADPY_DONE

                    str(getpadpy_key$,1,26) = str(getpadpy_rec$(),7,26)

                    delete #5

                    convert postseq% to str(getpadpy_rec$(),174,10), ~
                               pic(##########)


                    write #5, using GETPADPY_FMT1, getpadpy_rec$(),  ~
                               eod goto GETPADPY_NEXT


                            goto GETPADPY_NEXT

                               



         GETPADPY_DONE
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
            call "SETPRNT" ("CRC004", " ", 0%, 1%)
            end
