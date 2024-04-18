        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  RRRR    GGG    SSS   TTTTT  RRRR    *~
            *  A   A  R   R    I    R   R  G      S        T    R   R   *~
            *  AAAAA  RRRR     I    RRRR   G GGG   SSS     T    RRRR    *~
            *  A   A  R   R    I    R   R  G   G      S    T    R   R   *~
            *  A   A  R   R  IIIII  R   R   GGG    SSS     T    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIRGSTR - PRINTS THE INVOICE REGISTER BASED ON RECORDS   *~
            *            PLACED IN THE FILE 'ARIREGTF'.                 *~
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
            * 09/29/86 ! Original                                 ! JIM *~
            * 11/11/87 ! Added Invoice type 'X' for EXPORT        ! MJB *~
            * 08/07/90 ! G/L export file modifications.           ! RAC *~
            * 03/25/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/09/93 ! PRR 12510 Now increments NBR_LINES% to   ! JIM *~
            *          !   cause page breaks.                     !     *~
            * 03/09/93 ! Added Time of Day to End of Report.      ! JIM *~
            *03/11/2013! (AWD001) mod for escamt                  ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            arijrltf_key$43,             /* Key to ARIJRLTF            */~
            billtoxref$9,                /* Bill to Xref from ARIREGTF */~
            bol$3,                       /* Bill of Lading frm ARIREGTF*/~
            company_name$60,             /* Company name from COMPNAME */~
            cust_code$9,                 /* Customer code from ARIREGTF*/~
            cust_name$30,                /* Customer name from ARIREGTF*/~
            date$8,                      /* Date for screen display    */~
            eodsw$1,                     /* End of ARIREGTF switch     */~
            invoice$8,                   /* Invoice # from ARIREGTF    */~
            invoice_type$1,              /* Invoice type from ARIREGTF */~
            itype$(9)4,                  /* Invoice type descriptions  */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            post_date$8,                 /* Posting date from ARIREGTF */~
            prntsw$1,                    /* Csontrols printing on/off  */~
            rptid$6,                     /* Report ID                  */~
            session$6,                   /* Session ID from ARIREGTF   */~
            session_brk$6,               /* Controls session breaks    */~
            session_desc$20,             /* Session description        */~
            so$16,                       /* Sales Order # from ARIREGTF*/~
            stor_code$3,                 /* Store code from ARIREGTF   */~
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
            * #3  ! ARIREGTF ! A/R Invoice Register Transaction File    *~
            * #9  ! ARIJRLTF ! A/R Invoicing Journal Transaction File   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "ARIREGTF",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  26                      ~

            select #9,  "ARIJRLTF",                                      ~
                        varc,     indexed,  recsize =  638,              ~
                        keypos =    1, keylen =  43                      ~

REM         call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
                rslt$(9 ) = "REQUIRED"
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

REM         call "SHOSTAT" ("Invoice Register(s) are being printed")
            call "COMPNAME" (12%, company_name$, max_lines%)
            max_lines% = 56%
            itype$(1) = "UNKN" : itype$(2) = "ADJ "
            itype$(3) = "CM  " : itype$(4) = "DIR "
            itype$(5) = "FIN " : itype$(6) = "RCR "
            itype$(7) = "MANL" : itype$(8) = "SO  "
            itype$(9) = "EXP "
            rptid$ = "ARI003"
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            time$ = time
            call "TIME" (time$)
            session_brk$ = xor session_brk$
            prntsw$, eodsw$ = "0"
            plowkey$ = userid$
            str(plowkey$,4) = all(hex(00))

        REM *************************************************************~
            *           M A I N   P R O G R A M   L O O P               *~
            *************************************************************

        plow_thru_ariregtf
            call "PLOWNXT1" (#3, plowkey$, 3%, f1%(3))
            if f1%(3) = 0% then goto end_of_report
            get #3 using L10120, session$, invoice$, cust_code$,          ~
                billtoxref$, cust_name$, invoice_type$, stor_code$, so$, ~
                bol$, discount, freight, salestax, netamount, escamt
                                                     /* (AWD001) */
        REM RECORD LAYOUT FOR FILE 'ARIREGTF' ***************************
L10120:         FMT  XX(3),              /* User ID                    */~
                     CH(6),              /* Session ID                 */~
                     CH(8),              /* Invoice number             */~
                     CH(9),              /* Customer code              */~
                     CH(9),              /* Bill to Xref               */~
                     CH(30),             /* Customer name              */~
                     CH(1),              /* Invoice type               */~
                     CH(3),              /* Store code                 */~
                     CH(16),             /* Sales Order number         */~
                     CH(3),              /* Bill of Lading number      */~
                     4*PD(14,4),         /* Disc, Freight, Stax, Net   */~
                     PD(14,4)            /* ESC AMT                    */

            if session$ <> session_brk$ then gosub end_of_session
            if nbr_lines% > max_lines% then gosub page_heading
            discount = round(discount, 2)
            freight  = round(freight , 2)
            salestax = round(salestax, 2)
            escamt   = round(escamt,2)                 /* (AWD001) */
            netamount = round(netamount, 2)
            print using L60180, invoice$, billtoxref$, cust_code$,        ~
                cust_name$, itype$(pos("ACDFGMOX"=invoice_type$) + 1%),  ~
                stor_code$, str(so$,1,8) & "-" & bol$, discount, freight,~
                salestax, escamt, netamount      /* (AWD001) */
            nbr_lines% = nbr_lines% + 1%
            rt_disc = rt_disc + discount
            rt_frgt = rt_frgt + freight
            rt_stax = rt_stax + salestax
            rt_eamt = rt_eamt + escamt          /* (AWD001) */
            rt_namt = rt_namt + netamount
            delete #3
            goto plow_thru_ariregtf

        end_of_session
            if prntsw$ = "0" then goto start_print
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60210 /* UNDERSCORES */              /* (AWD001) */
            print using L60240, rt_disc, rt_frgt, rt_stax, rt_eamt, rt_namt
            rt_disc, rt_frgt, rt_stax, rt_eamt, rt_namt = 0
            print
            time$ = " " : call "TIME" (time$)
            print using L60270, time$
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
        start_print
            if eodsw$ = "1" then return
            page_nbr% = 0% : nbr_lines% = 99%
            prntsw$ = "1"
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            session_brk$ = session$
            post_date$, session_desc$ = " "
            arijrltf_key$ = str(userid$,,3) & str(session$,,6)
            call "READ100" (#9, arijrltf_key$, f1%(9))
            if f1%(9) = 0% then return
            get #9 using L10640, post_date$, session_desc$
L10640:         FMT  POS(45), CH(6), CH(20)
            call "DATEFMT" (post_date$)
            return

        end_of_report
            eodsw$ = "1"
            gosub end_of_session
            goto exit_program

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 7%
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, post_date$, page_nbr%
            print using L60100, session$, session_desc$
            print skip
            print using L60110
            print using L60120, "#"
            print using L60150 /* UNDERSCORES */
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
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
/* (AWD001) Changes to print stmts to add escamt */
L60040: %RUN DATE: ######## @ ########        ###########################~
        ~#################################                    ARIRGSTR####~
        ~###
L60070: %    POST: ########                                  I N V O I C ~
        ~E   R E G I S T E R                                       PAGE: #~
        ~###
L60100: % SESSION: ###### ####################
L60110: %INVOICE  BILL-TO   SHIP-TO                          INV.
L60120: %NUMBER   NUMBER    NUMBER    SHIP-TO NAME (SHORT)   TYPE STR SO ~
        ~#   -BOL   DISCOUNT    FREIGHT  SALES TAX   ENERGY        NET AMO~
        ~UNT

L60150: %-------- --------- --------- ---------------------- ---- --- ---~
        ~--------- ---------- ---------- ---------- ---------- -----------~
        ~---
L60180:    %######## ######### ######### ###################### #### ### ~
        ~############ -##,###.## -##,###.## -##,###.## -##,###.## -##,###,~
        ~###.##

L60210: %                                                                ~
        ~          ---------- ---------- ---------- ---------- -----------~
        ~---

L60240: %                                                  *** REPORT TOT~
        ~ALS:     -###,###.##-###,###.##-###,###.##-###,###.##-###,###,###~
        ~.##

L60270: %                                                   ** END OF REP~
        ~ORT @ ######## **

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
