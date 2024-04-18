        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  PPPP   RRRR   IIIII  N   N  TTTTT   *~
            *  A   A  R   R    I    P   P  R   R    I    NN  N    T     *~
            *  AAAAA  RRRR     I    PPPP   RRRR     I    N N N    T     *~
            *  A   A  R   R    I    P      R   R    I    N  NN    T     *~
            *  A   A  R   R  IIIII  P      R   R  IIIII  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIPRINT - PASSES DATA TO THE SUBROUTINE 'ARIPRSUB',      *~
            *            WHICH THEN PRINTS CUSTOMER INVOICES.           *~
            *            DATA SELECTED ARE ALL RECORDS IN 'ARIINVRF'    *~
            *            FOR THE CURRENT USER. DATA PASSED ARE THE      *~
            *            RELATED CUSTOMER CODE AND INVOICE NUMBER.      *~
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
            * 10/14/86 ! Original                                 ! JIM *~
            * 04/06/93 ! PRR 12404. No longer relies on record cnt! JDH *~
            * 03/05/98 ! Mod - Special Version of 'ARIPRSUB' (EWD)! RHH *~
            * 05/08/00 ! Add sub 'ARIPRS2' for auto faxing of     ! CMG *~
            *          !    invoices  (EWD001)                    !     *~
            * 03/08/01 ! Mod to put Lowe's, Norandex & Reynolds   ! CMG *~
            *          !    invoices  into fax file (EWD002)      !     *~
            * 04/04/06 ! Mod to put IBSA invoices into fax file   ! CMG *~
            *          !   (AWD003)                               *     *~
            * 08/17/06 ! Mod to put Strober in fax file or anythin! CMG *~
            *          !  with sku code  (AWD004)                 !     *~
            * 09/14/06 ! change program to abend on DB connection ! DES *~
            *          !  failure (AWD005)                        !     *~
            *05/23/2007! AWD006 - skip printing invoices for edi  ! DES *~
            *03/24/2009! AWD007 - skip printing invoices for edi  ! DES *~
            *01/07/2010! (AWD008) - mod for skiping invoice with  ! CMG *~
            *          !        SSS problem                       !     *~
            *06/03/2011! (AWD009) - add orcl usr & pswd lookup    ! CMG *~
            *06/07/2013! (AWD010) - add edi code check for        ! CMG *~
            *          !    McCoys and Do it Best                 !     *~
            *05/06/2016! (SR74733) send sku 600 to EDI print      ! CMG *~
            *12/28/2016! (SR78829) mod to run off printing by table!CMG *~
            *05/06/2019! CR1993 read PGPO to print in separate rpt !RDB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        REM *************************************************************~
            *THIS PROGRAM CALLS THE SUBROUTINE 'ARIPRSUB' - PRINT CUS-  *~
            *TOMER INVOICES.                                            *~
            *************************************************************

        dim                                                              ~
            cust_code$9,                 /* Customer code from ARIINVRF*/~
            hdr$40,                      /* ASKUSER constant           */~
            invoice$8,                   /* Invoice # from ARIINVRF    */~
            msg$(3)79,                   /* ASKUSER messages           */~
            plowkey$20,                  /* Current user/ARIINVRF key  */~
            rptid$6,                     /* Report ID                  */~
            userid$3,                    /* Current user               */~
            pgpo$20,                     /* PlyGem PO                  */~
            sku$3                        /* Sku Code                   */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        dim fax_flag$1,                  /* Flag to fax or not         */~
            ari_key$17,                  /* ARI Master Key             */~
            inv_type$1,                  /* Invoice Type               */~
            inv_no$10,                   /* CUSTOMER Phone No.         */~
            ewdkey$20                    /* Update For Faxed I/C/A     */
/*  (EWD001)   */

        dim                                                              ~
            server$25,                   /* Connection String          */~
            user$25,                     /* User name to Connect       */~
            pass$25                      /* Password to Connect        */

/*(AWD008) */
        dim so$8,                        /* Sales Order number         */~
            readkey$100                  /* Gencodes Readkey           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim key_data$90
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

            mat f2% = con : mat f1% = zer  :  cnt% = 0%  :  faxes% = 0%
            plycnt% = 0%  : plygem% = 0%
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3  ! ARIINVRF ! A/R Invoice Repost file                  *~
            * #08 ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "ARIINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

            select #4, "CUSTOMER",                                       ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            select #5,  "EWDINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

            select #6, "ARIMASTR",                                       ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup,        ~
                            key 4, keypos = 1783, keylen = 26

/*(AWD008)*/
            select #7, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

           select #08,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

                        
            select #9,  "EWDPLYRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

                        
REM         call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%,   rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%,   rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 100%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%,   rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%,   rslt$(7 ))
/*(AWD009) */
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 100%, rslt$(9 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            comp% = 0% : first_time% = 1%  :  ewdinvrf% = 0% : ewdplyrf% = 0%
            pg_flag% = 0%       /* CR1993 */
            call "EXTRACT" addr("ID", userid$)
     /*     userid$ = "RGB"      Testing  */
            plowkey$ = userid$
            str(plowkey$,4) = all(hex(00))
            goto L10000  /* Bypass OPEN_PRINTER for now */

        open_printer
            if comp% > 0% then L09240
L09140:         comp% = 0%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Invoice data to print"
                msg$(3) = "Press RETURN to cancel program"
REM             call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
REM             if comp% <> 0% then L09140
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09240:     dup%, prt%, abend% = 0%
            rptid$ = "ARI006"
            select printer
            return

L10000: REM *************************************************************~
            *           M A I N   P R O G R A M   L O O P               *~
            *************************************************************

        main_program_loop
            call "PLOWNEXT" (#3, plowkey$, 3%, f1%(3))
            if f1%(3%) = 1% then comp% = 1% /* There was at least 1 rec */
            if first_time% = 1% then gosub open_printer
            if first_time% = 1% then gosub oracle_connect
            first_time% = 0%
            if f1%(3%) = 0% then goto end_of_job
            get #3 using L10100, cust_code$, invoice$

        REM RECORD LAYOUT FOR FILE 'ARIINVRF' ***************************
L10100:         FMT  XX(3),              /* User ID                    */~
                     CH(9),              /* Customer code              */~
                     CH(8)               /* Invoice number             */


            gosub check_invoice                      /* (AWD008)  */
            if sssInv% = 1% then goto main_program_loop
                                                     /*  (EWD001)   */
            gosub check_customer
            if ewdinvrf% = 1% then goto L10200       /*  (EWD002)   */
            if fax_flag$ <> "Y" then goto L10205
               call "ARIPRSU3" (dup%, prt%, abend%, cust_code$, invoice$,~
                                rptid$, inv_no$)

L10200:        cnt% = cnt% + 1%
               init(" ") inv_no$
               gosub update_ewdinvrf
                   if abend% <> 0% then goto abend_program  /* Did it abort? */
                   goto main_program_loop

L10205:        if pgpo$ > " " then gosub update_ewdplyrf  /* counter in update*/
               if pgpo$ > " " then goto main_program_loop
               
               call "ARIPRSUB" (dup%, prt%, abend%, cust_code$, invoice$,~
                                rptid$, inv_no$)
                                          /*  (EWD001)   */
L10209:     if abend% <> 0% then goto abend_program  /* Did it abort? */
            goto main_program_loop                   /* No- continue */

        end_of_job
            if prt% <> 0% then goto L10280 /* Print anything? */
L10210:         comp% = 0%                /* No- so inform the user */
                hdr$ = "*** NULL SET OF INVOICES ***"
                msg$(1) = "There were no Invoices to be printed for " &  ~
                     "user " & userid$
                msg$(3) = "Press RETURN to continue"
REM             call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
REM             if comp% <> 0% then goto L10210
L10280:     plowkey$ = userid$ /*Delete current user's invoice records*/
            str(plowkey$,4) = all(hex(00))
            call "DELETE" (#3, plowkey$, 3%)
        abend_program
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto exit_program
        
        check_customer                                    /*  (EWD001)   */
            init(" ") inv_no$, fax_flag$, sku$
            inv_no%, partners2% = 0%
            fax_flag$ = "N"                               /*  (EWD002)  */
            gosub check_inv_type

/* Print in normal print file (for Form print) NO & RE crd and adj  */
            if inv_type$ <> "C" and inv_type$ <> "A" then goto L10350
               goto L10320

L10350:     read #4, key = cust_code$, using L10300, inv_no$, sku$,  ~
                                                   eod goto L10310
L10300:       FMT POS(228), CH(10), POS(1000), CH(3)      /* (AWD004) */

/* Has to be invoice  */
REM            if str(cust_code$,1%,2%) = "LO" then goto L10325
REM            if str(cust_code$,1%,2%) = "NO" then goto L10325
REM            if str(cust_code$,1%,2%) = "RE" then goto L10325
REM            if str(cust_code$,1%,2%) = "PL" then goto L10325
* (AWD003)
REM            if str(cust_code$,1%,2%) = "IB" then goto L10325

* (AWD004)  note do not do for all customers with sku code b/c
*   we run parallel until customer is ready to only receive EDI
* Do not make customer code specific anymore like above caused problems

            if sku$ = "001" then goto L10325   /* Lowes */
            if sku$ = "002" then goto L10325
            if sku$ = "003" then goto L10325
            if sku$ = "005" then goto L10325

            if sku$ = "004" then goto L10325   /* Norandex */
            if sku$ = "010" then goto L10325

            if sku$ = "020" then goto L10325   /* Plymart  */
            if sku$ = "030" then goto L10325   /* IBSA  */
            if sku$ = "040" then goto L10325   /* Strober  */

/* <AWD006> */
            /* Guardian  */
            if sku$ = "042" then goto L10325
REM            if sku$ = "043" then goto L10325
            /* Lansing   */
            if sku$ = "044" then goto L10325
            /* 84 Lumber */
            if sku$ = "048" then goto L10325
            /* ENAP  AWD007    */
            if sku$ = "050" then goto L10325
            if sku$ = "057" then goto L10325 /* Carter */
            if sku$ = "500" then goto L10325
/* </AWD006> */
/* (AWD010) */
            if sku$ = "058" then goto L10325
            if sku$ = "060" then goto L10325
REM Corporate Sent Invoices
            if sku$ = "061" then goto L10325
REM Internal Accounts
            if sku$ = "062" then goto L10325
/* (\AWD010) */
/* SR74733 */
            if sku$ = "600" then goto L10325

            if str(cust_code$,1%,2%) = "CY" and                  ~
                      str(sku$,1%,3%) <> " " then goto L10325
/* (SR78829) */
            gosub checkPartners2
            if partners2% = 1% then goto L10325
/* (SR78829) */


            if str(inv_no$,1%,10%) <> " " then fax_flag$ = "Y"
            call "SPCSMASH" (inv_no$)
            if len(inv_no$) < 7% then fax_flag$ = "N"

            convert inv_no$ to inv_no%, data goto L10320

L10310: return
L10325:     ewdinvrf% = 1%
L10320:     fax_flag$ = "N"                            /*  (EWD001)   */
            str(inv_no$,1%,11%) = " "
        return

        update_ewdinvrf
            init(" ") ewdkey$ : ewdinvrf% = 0%
            str(ewdkey$,1%,3%)  = userid$
            str(ewdkey$,4%,9%)  = cust_code$
            str(ewdkey$,13%,8%) = invoice$

            read #5, hold, key = ewdkey$, eod goto not_there

                 delete #5
        not_there
            put #5, using L10330, userid$, cust_code$, invoice$

L10330:         FMT CH(03), CH(09), CH(08)

            write #5, eod goto ewd_error

        return
            
        ewd_error
        init(" ") key_data$
    str(key_data$,01,01) = hex(22)
    str(key_data$,02,60) =                                 ~
                         "Upable to update EWDINVRF : " & invoice$
    str(key_data$,78,01) = hex(22)
        call "ARIMAIL" (key_data$)
    stop
         call "SHOSTAT" (" Unable to update EWDINVRF : "  & invoice$) : stop
        return
        
/* CR1993 */        
        update_ewdplyrf
            init(" ") ewdkey$ : ewdplyrf% = 0%
            str(ewdkey$,1%,3%)  = userid$
            str(ewdkey$,4%,9%)  = cust_code$
            str(ewdkey$,13%,8%) = invoice$
            plycnt% = plycnt% + 1%

            read #9, hold, key = ewdkey$, eod goto not_arec

                 delete #9
        not_arec
            put #9, using L10335, userid$, cust_code$, invoice$

L10335:         FMT CH(03), CH(09), CH(08)

            write #9, eod goto plyewd_error

        return

        plyewd_error
        init(" ") key_data$
    str(key_data$,01,01) = hex(22)
    str(key_data$,02,60) =                                 ~
                         "Upable to update EWDPLYRF : " & invoice$
    str(key_data$,78,01) = hex(22)
        call "ARIMAIL" (key_data$)
    stop
         call "SHOSTAT" (" Unable to update EWDPLYRF : "  & invoice$) : stop
        return
        
        check_inv_type
             init(" ") ari_key$
             str(ari_key$,1%,9%)  = cust_code$
             str(ari_key$,10%,8%) = invoice$
                                                  /* (AWD008) */
             read #6, key = ari_key$, using L10340, so$, inv_type$,    ~
                                                    pgpo$,             ~
                                                eod goto inv_type_done
L10340:        fmt pos(34), ch(8), pos(891), ch(1), pos(1844), ch(20)

        inv_type_done
        return

        print_faxed
            plowkey$ = userid$
            str(plowkey$,4) = all(hex(00))
            faxes% = 1%
            inv_no$ = " F A X E D "
            comp%, cnt% = 0%  :  first_time% = 1%
        print_faxed_main
            call "PLOWNEXT" (#5, plowkey$, 3%, f1%(5))
            if f1%(5%) = 1% then comp% = 1% /* There was at least 1 rec */
            if first_time% = 1% then gosub open_printer
            first_time% = 0%
            if f1%(5%) = 0% then goto end_of_job_fax
            get #5 using L10100, cust_code$, invoice$

            call "ARIPRSUB" (dup%, prt%, abend%, cust_code$, invoice$,~
                                rptid$, inv_no$)
                                          /*  (EWD001)   */
            if abend% <> 0% then goto abend_program_fax  /* Did it abort? */
            goto print_faxed_main                        /* No- continue */

/* CR1993 */
        print_plygem
            plowkey$ = userid$
            str(plowkey$,4) = all(hex(00))
            plygem% = 1%
            init(" ") inv_no$ 
            comp%, plycnt% = 0%  :  first_time% = 1%
        print_plygem_main
            call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            if f1%(9%) = 1% then comp% = 1% /* There was at least 1 rec */
            if first_time% = 1% then gosub open_printer
            first_time% = 0%
            if f1%(9%) = 0% then goto end_of_plygem
            get #9 using L10100, cust_code$, invoice$

            call "ARIPRSUB" (dup%, prt%, abend%, cust_code$, invoice$,~
                                rptid$, inv_no$)

            if abend% <> 0% then goto abend_plygem  /* Did it abort? */
            goto print_plygem_main                        /* No- continue */

        oracle_connect                                     /*(EWD005)*/
            init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD009) */

            oci_err% = 0%
            call "CONNECT"  (user$, pass$, server$, oci_err%)
/*AWD005*/
        if oci_err% >= 0 then return
            msg$(1) = "Database connection failed"
        msg$(2) = "Program abending          "
            msg$(3) = "Press RETURN to continue"
REM  call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        goto abend_program_fax
/*AWD005*/
        return

        oracle_discnnct
            call "DISCNNCT" (oci_err%)
        return


/* (AWD009) beg */
        get_user_pswd
            call "READ100" (#08, "ORACLE PASSWORD", f1%(08%))   /* SYSFILE2 */
            if f1%(08%) <> 0% then get #08 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return
/* (AWD009) END */

/*(AWD008) */
        check_invoice

            gosub check_inv_type
            sssInv% = 0%
            init(" ") readkey$
            readkey$ = "SSSORDERS" & so$

            read #7, key = readkey$, eod goto goodInv

              sssInv% = 1%
        goodInv
        return

/*(SR78829)*/
        checkPartners2
          partners2% = 0%
          init(" ") readkey$
          readkey$ = "PARTNERS2" & sku$

          read #7, key = readkey$, eod goto noPartners2

              partners2% = 1%
        noPartners2
        return

/*(SR78829)*/

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

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
            if cnt% = 0% then goto L65035
               gosub print_faxed

            end_of_job_fax
            if prt% <> 0% then goto L65010 /* Print anything? */
L65020:         comp% = 0%                /* No- so inform the user */
                hdr$ = "*** NULL SET OF INVOICES ***"
                msg$(1) = "There were no Invoices to be faxed for  " &  ~
                     "user " & userid$
                msg$(3) = "Press RETURN to continue"
REM             call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
REM             if comp% <> 0% then goto L65020
L65010:     plowkey$ = userid$ /*Delete current user's invoice records*/
            str(plowkey$,4) = all(hex(00))
            if faxes% = 1% then call "DELETE" (#5, plowkey$, 3%)
        abend_program_fax
        
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            
L65035:     if plycnt% = 0% then goto L65000 
            pg_flag%  = 1%  
            gosub print_plygem

            end_of_plygem
            
            if prt% <> 0% then goto L65015 /* Print anything? */
L65025:         comp% = 0%                /* No- so inform the user */
                hdr$ = "*** NULL SET OF INVOICES ***"
                msg$(1) = "There were no Invoices to be faxed for  " &  ~
                     "user " & userid$
                msg$(3) = "Press RETURN to continue"
REM             call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
REM             if comp% <> 0% then goto L65025
L65015:     plowkey$ = userid$ /*Delete current user's invoice records*/
            str(plowkey$,4) = all(hex(00))
            if plygem% = 1% then call "DELETE" (#9, plowkey$, 3%)            
            
        abend_plygem
            
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

L65000:     gosub oracle_discnnct
REM         call "SHOSTAT" ("One Moment Please")
            end

