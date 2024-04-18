        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   CCC   RRRR   H   H  L      DDDD    *~
            *  B   B  C   C  K  K   C   C  R   R  H   H  L      D   D   *~
            *  BBBB   C      KKK    C      RRRR   HHHHH  L      D   D   *~
            *  B   B  C   C  K  K   C   C  R   R  H   H  L      D   D   *~
            *  BBBB    CCC   K   K   CCC   R   R  H   H  LLLLL  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKCRHLD - PRINTS A SUMMARY LISTING OF BILL-TO CUSTOMERS  *~
            *            WITH ORDERS IN CREDIT HOLD STATUS.             *~
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
            * 09/03/86 ! Original                                 ! JIM *~
            * 11/09/88 ! Teroson credit control family added.     ! JDH *~
            * 12/01/88 ! Family Exposure shown for all Customers. ! JDH *~
            * 10/10/89 ! Changed where the Credit Parent was got. ! JDH *~
            * 04/08/91 ! PRR 11811  Rounding Error Bill-TO        ! SID *~
            *          !            EXPOSURE TO FAMILY EXPOSURE   !     *~
            *          ! Prevent cancelled orders being counted   !     *~
            *          !            as an open orders held.       !     *~
            * 05/24/91 ! PRR 11883, 11313 Excl SOs w/ OpenAmt = 0 ! JIM *~
            * 05/28/91 ! ALLFREE.                                 ! JIM *~
            * 04/20/92 ! If Cr Hold Flag set orders show up here. ! JDH *~
            *          !   This negates work done on 05/24/91.    !     *~
            * 11/13/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 01/05/93 ! Amount Held rounded per BCKCRINP.        ! JIM *~
            * 04/06/93 ! PRR 12860. A year later I'll try again.  ! JDH *~
            *          !   If no line items have any open qty then!     *~
            *          !   don't include 'H'eld SO in this report.!     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            ar(2), oo(2),                /* Amounts for family exposure*/~
            asterisk$1,                  /* Asterisk of space for print*/~
            bck_crhld$1,                 /* Credit hold flaf (BCKMASTR)*/~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            billto_brk$9,                /* Bill-to break control      */~
            company_name$60,             /* Name of company (COMPNAME) */~
            cus_billnm$30,               /* Customer ship-to name      */~
            cus_billto$9,                /* Bill-to customer code      */~
            cus_shipnm$30,               /* Ship-to customer name      */~
            cus_shipto$9,                /* Current ship customer code */~
            date$8,                      /* Date for screen display    */~
            eojsw$1,                     /* End of job switch          */~
            parent_code$9,               /* Credit Parent Cross Ref    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            time$8,                      /* Time of day stamp          */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
            * #1  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #2  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #3  ! CCRMASTR ! Customer Credit Master file              *~
            * #4  ! BCKLINES ! Back Log Line Item File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #4,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

L09060:     u3% = 2%
            call "ASKUSER" (u3%, "*** CREDIT HOLD LISTING ***",          ~
                            "Press RETURN to continue with this report", ~
                            "-- OR --",                                  ~
                            "Press PF(16) to cancel and return to menu")
            if u3% = 16% then goto exit_program
            if u3% <> 0% then goto L09060

            call "SHOSTAT" ("Printing Credit Hold Listing")
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            time$ = " " : call "TIME" (time$)
            rptid$ = "BCK003"
            call "COMPNAME" (12%, company_name$, ret%)
            ret% = ret%
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            page_nbr% = 0% : gosub page_heading /* FORCE MINIMUM REPORT*/
            init(hex(00)) plowkey$, eojsw$
            one_time%, rt_nbrheld%, rt_amtheld = 0


        REM *************************************************************~
            *          M A I N   P R O C E S S I N G   L O O P          *~
            *-----------------------------------------------------------*~
            * READS, ACCUMULATES AND PRINTS THE REPORT.                 *~
            *************************************************************

        REM 'PRIME' THE CUSTOMER MASTER FILE FOR LATER SEQUENTIAL READS *
            call "REDALT2" (#1, plowkey$, 4%, f1%(1))
            if f1%(1) = 0% then goto report_total
            goto process_customer /* SKIP INITIAL 'READNEXT' */

        read_thru_customer
        REM *************************************************************~
            * READS THE CUSTOMER MASTER FILE ON ALTERNATE KEY #4, THE   *~
            * BILL-TO CROSS-REFERENCE CODE.                             *~
            *************************************************************
            call "READNEXT" (#1%, f1%(1))
            if f1%(1) = 0% then report_total /* END OF CUSTOMER FILE */
        process_customer
            call "READ100" (#3, key(#1), f1%(3%))/* Get related CCRMASTR*/
            get #1 using L10200, cus_billto$
L10200:         FMT POS(780), CH(9)     /* Bill-to X-ref */
            if one_time% = 1% then continue_process
                gosub print_billto_reset
                one_time% = 1%
        continue_process
            if cus_billto$ <> billto_brk$ then gosub print_billto_cust
            get #1 using L10280, cus_shipto$, cus_shipnm$
L10280:         FMT  CH(9),                /* Ship-to Customer code    */~
                     CH(30)                /* Ship-to Customer name    */
            cus_sopen, cus_aropn = 0                            /* JIC */
            if f1%(3%) <> 0% then get #3 using L10300, cus_sopen, cus_aropn
L10300:         FMT POS(122), 2*PD(14,4)  /* Ship-to, A/R open amounts*/
            t1_aropn = t1_aropn + cus_aropn
            t1_sopen = t1_sopen + cus_sopen
            if cus_billto$ <> cus_shipto$ then bckmastr_loop
                get #1 using L10350, cus_billnm$, cus_crlim, parent_code$
L10350:              FMT  POS(10), CH(30),   /* Bill-to Customer name  */~
                          POS(526), PD(14,4),/* Credit limit           */~
                          POS(1049), CH(9)   /* Credit Parent          */
                cus_bopen = 0                                   /* JIC */
                if f1%(3%) <> 0% then get #3 using L10378, cus_bopen
L10378:              FMT POS(114), PD(14,4)  /* Bill-to open           */
        bckmastr_loop
            bckmastr_key$ = key(#1%) & hex(00)
        bckmastr_read
            call "PLOWNEXT" (#2%, bckmastr_key$, 9%, f1%(2))
            if f1%(2) = 0% then bckmastr_test_print
            get #2 using L10460, bck_disc, bck_open, bck_crhld$

        REM PARTIAL RECORD LAYOUT FOR FILE 'BCKMASTR' *******************
L10460:         FMT  POS(859), PD(14,4), /* Order-level discount percnt*/~
                     POS(867), PD(14,4), /* Gross amount open          */~
                     CH(1)               /* Credit hold flag ('H'=held)*/

*          IF BCK_OPEN = 0 THEN GOTO BCKMASTR_READ
            bck_open = bck_open - ((bck_open * bck_disc) / 100)
            t1_billto = t1_billto + bck_open
            t1_shipto = t1_shipto + bck_open
            if bck_crhld$ <> "H" then goto bckmastr_read
            qty_open = 1 /* Prime it in case we don't go to LINE_TEST */
            if bck_open = 0 then gosub line_test
            if qty_open = 0 then goto bckmastr_read
            t1_nbrheld% = t1_nbrheld% + 1%
            t1_amtheld  = round(t1_amtheld + bck_open, 0)
            t1_shpheld% = t1_shpheld% + 1%
            t1_shpamt   = t1_shpamt + bck_open
            goto bckmastr_read

        line_test
            qty_open = 0
            plowkey2$ = str(bckmastr_key$, 10%, 16%)
L10590:     call "PLOWNEXT" (#4, plowkey2$, 16%, f1%(4%))
            if f1%(4%) = 0% then return
                get #4 using L10596, qty_open
L10596:              FMT POS(109), PD(14,4)
                if qty_open > 0 then return /* Got something, continue */
                goto L10590

        bckmastr_test_print
        REM *************************************************************~
            * DELETE THE FOLLOWING 'GOTO' STATEMENT TO INSTALL SHIP-TO  *~
            * VALIDATION. Note-No Credit Family mods have been made here*~
            *************************************************************
            goto read_thru_customer      /* THIS IS THE ONE */
            asterisk$ = "!"
            if t1_shipto <> cus_sopen then goto shipto_print_it
            asterisk$ = "S"
            if t1_shpheld% = 0% then goto bckmastr_test_reset
        shipto_print_it
            if line_nbr% > 56% then gosub page_heading
            cus_aropn  = round(cus_aropn,  0)
            cus_sopen  = round(cus_sopen, 0)
            t1_shpamt = round(t1_shpamt, 0)
            print using L60210, cus_shipto$, cus_shipnm$, 0, cus_aropn,   ~
                cus_sopen, asterisk$, cus_aropn + cus_sopen, 0,          ~
                t1_shpheld%, t1_shpamt
            line_nbr% = line_nbr% + 1%
         bckmastr_test_reset
            t1_shpheld%, t1_shpamt, t1_shipto = 0
            goto read_thru_customer

        print_billto_cust
            rt_nbrheld% = rt_nbrheld% + t1_nbrheld%
            rt_amtheld  = rt_amtheld  + t1_amtheld
            asterisk$ = "*"
            if t1_sopen <> cus_bopen then billto_print_it
            asterisk$ = " "
            if t1_nbrheld% = 0% then print_billto_reset
        billto_print_it
            gosub family_expsr
            if par% = 1% then parent_code$ = "*** P ***"
            if line_nbr% > 56% then gosub page_heading
            cus_crlim  = round(cus_crlim, 0)
            t1_aropn   = round(t1_aropn,  0)
            cus_bopen  = round(cus_bopen, 0)
            t1_expsr   = round(t1_aropn + cus_bopen, 0)
            if parent_code$ <> " " then L11105
                t1_ovlim   = round(t1_expsr - cus_crlim, 0)
                goto L11120
L11105: REM IF PAR% = 1% THEN                                            ~
        REM     T1_OVLIM   = ROUND(FAM_EXPSR - CUS_CRLIM, 0) ELSE        ~
        REM     T1_OVLIM   = ROUND(OO(2) + AR(2) - CUS_CRLIM, 0)
                t1_ovlim   = round(fam_expsr - cus_crlim, 0)
L11120:     t1_amtheld = round(t1_amtheld, 0)
        REM  IF PAR% = 0% THEN FAM_EXPSR = 0
            print using L60210, billto_brk$, cus_billnm$, parent_code$,   ~
                cus_crlim, t1_aropn, cus_bopen, asterisk$, t1_expsr,     ~
                fam_expsr, t1_ovlim, t1_nbrheld%, t1_amtheld

            line_nbr% = line_nbr% + 1%
        print_billto_reset
            if eojsw$ = "1" then return
            cus_billnm$ = "CUSTOMER NOT IN FILE"
            cus_crlim, cus_aropn, cus_bopen, cus_sopen, t1_expsr,        ~
                t1_aropn, t1_nbrheld%, t1_amtheld, t1_billto, t1_sopen,  ~
                cl, fam_expsr, t1_ovlim, bck_open = 0
            mat ar = zer : mat oo = zer
            billto_brk$ = cus_billto$
            parent_code$ = " "
            return

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 7%
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, page_nbr%
            print
            print using L60100
            print using L60160
            print using L60130
            print
            return

        report_total
            eojsw$ = "1"  /* INDICATE END OF JOB */
            gosub print_billto_cust
            if line_nbr% > 56 then gosub page_heading
            rt_amtheld = round(rt_amtheld, 0)
            print
            print using L60190, rt_nbrheld%, rt_amtheld
            print skip(2)
            time$ = " " : call "TIME" (time$)
            print using L60230, time$                 /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto exit_program

        family_expsr        /* Get info on the Credit Family */
            par% = 0%
            call "BCKCRDSB" (billto_brk$, oo(), ar(), cl, par%)
            cus_crlim = cl
            oo(2) = round(oo(2), 0) : ar(2) = round(ar(2), 0)
            fam_expsr = round(oo(2) + ar(2), 0)
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                    BCKCRHLD######~
        ~#
L60070: %                                                        CREDIT H~
        ~OLD LISTING                                             PAGE: ###~
        ~#
L60100: %                                         CREDIT       CREDIT    ~
        ~            OPEN      BILL-TO     FAMILY     AMOUNT ORDRS     AMO~
        ~UNT

L60130: %--------- ------------------------------ --------- --------- ---~
        ~------ ---------   ---------- ---------- ---------- ----- -------~
        ~---

L60160: %BILL-TO   BILL-TO NAME                   PARENT        LIMIT  OP~
        ~EN A/R    ORDERS     EXPOSURE   EXPOSURE OVER LIMIT  HELD       H~
        ~ELD

L60190: %**** REPORT TOTALS: ###,###,### ORDERS ARE ON CREDIT HOLD FOR $#~
        ~##,###,###
L60210: %######### ############################## ######### ######### -##~
        ~###### -######## # -######### -######### -######### ##### -######~
        ~###
L60230: %                                                    ** END OF RE~
        ~PORT @ ######## **

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
            call "ALLFREE"
            end
