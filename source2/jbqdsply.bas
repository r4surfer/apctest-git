        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ   DDDD    SSS   PPPP   L      Y   Y   *~
            *    J    B   B  Q   Q  D   D  S      P   P  L       Y Y    *~
            *    J    BBBB   Q   Q  D   D   SSS   PPPP   L        Y     *~
            *  J J    B   B  Q   Q  D   D      S  P      L        Y     *~
            *   J     BBBB    QQQ   DDDD    SSS   P      LLLLL    Y     *~
            *                    Q                                      *~
            *-----------------------------------------------------------*~
            * JBQDSPLY - Performs nearly all display functions for      *~
            *            CMS II Shop Floor Control.                     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/09/87 ! ORIGINAL                                 ! ERN *~
            * 03/23/88 ! Added Credit Ledger Display S/R JBQCREDT ! MDE *~
            * 06/10/88 ! Added Shostat Message @ 65000            ! MDE *~
            * 12/13/88 ! Add CDA trans inquiry 'CDAQJI'           ! ERN *~
            * 09/06/90 ! Took out 2nd PIPOUT channel to JBQMATL.  ! JDH *~
            * 07/02/92 ! Added PIPIN channel is pass to GETDEM    ! WPH *~
            * 04/01/93 ! Added JBMASTRC channel for core value    ! WPH *~
            *          ! and passed it to various subroutines     !     *~
            * 02/11/94 ! Added call to POSTATUS if PJ Job.        ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            date$8,                      /* Date for Screen Display    */~
            dem$19, demtype$1,           /* Calling Arguments          */~
            errormsg$79,                 /* Error Message              */~
            hdr$38,                      /* Screen Headers             */~
            job$8, jobdescr$32,          /* Job Number & Description   */~
            inpmessage$79,               /* Input Message              */~
            line2$79,                    /* 2nd Screen Line            */~
            pfkeys$32,                   /* PF Keys Available          */~
            po$16,                       /* Purchase Order             */~
            poline$3,                    /* Purchase Order Line Seq #  */~
            opt$(16)38,                  /* Selection Options          */~
            tagnr$19,                    /* PIP Tag Number for Job     */~
            text$(196,1)70, textid$4,    /* Job Text array and ID      */~
            type$1,                      /* Type of Call               */~
            vendor$9,                    /* Vendor Code                */~
                                                                         ~
            f1%(64),                     /* Record-on-File Flags       */~
            f2%(64)                      /* File Status Flags          */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  1  ! DEMMASTR ! Demand Master                            *~
            *  3  ! HNYMASTR ! Inventory Master                         *~
            *  4  ! JBMASTR2 ! Job Master                               *~
            *  5  ! JBMATER2 ! Job Material Ledger                      *~
            *  6  ! JBVALUE  ! Job Value Added (labor, misc, & WC)      *~
            *  7  ! JBCREDIT ! Job Credits Ledger                       *~
            *  8  ! JBSTATUS ! Job Status Tracking                      *~
            *  9  ! CDAAUDJI ! CDA JI Transaction Audit File            *~
            *  10 ! RTEMASTR ! Standard & Alt Work Center Routings      *~
            *  11 ! WCMASTR  ! Work Center Master                       *~
            *  12 ! JBMASTRC ! JBMASTR2 Core Appendix File              *~
            * #13 ! VBKLINES ! Purchase Order Line Item File            *~
            * #14 ! RCVLINES ! Receiver Line Items                      *~
            * #15 ! VBKMASTR ! Vendor Backlog Master File               *~
            * #16 ! PAYLINES ! Payables Line Items File                 *~
            * #17 ! PAYMASTR ! Payables Master File                     *~
            *  18 ! BOMMASTR ! Bills of Materials Structures            *~
            *  19 ! ENGMASTR ! BOM and Rte Effectivity Dates            *~
            * #20 ! TXTFILE  ! System Text File                         *~
            *  23 ! WCOUT    ! Work Center Cross Reference              *~
            *  34 ! PIPOUT   ! Planned Position Out                     *~
            *  35 ! PIPCROSS ! Hard Peg Cross Reference                 *~
            *  45 ! JBCROSS2 ! Job RTE/BOM used cross ref.              *~
            *  54 ! SYSFILE2 ! System File                              *~
            *  55 ! PIPMASTR ! PIP MASTER FILE                          *~
            *  56 ! SFCUM2   ! CUM FORCAST FILE                         *~
            *  57 ! HNYDETAL ! INVENTORY DETAILS FILE                   *~
            *  58 ! PIPIN    ! EXPECTED ADDTN'S TO INVENTORY            *~
            *  59 ! CALMASTR ! PLANNING CALENDER FILE                   *~
            *  60 ! HNYQUAN  ! Inventory Quantity File                  *~
            *  61 ! HNYPOOL  ! Inventory Pool File                      *~
            *************************************************************


            select #1, "DEMMASTR",                                       ~
                       varc, indexed, recsize = 123,                     ~
                       keypos = 2, keylen = 27,                          ~
                       alt key 1, keypos =10, keylen = 19,               ~
                           key 2, keypos = 1, keylen = 28

            select #3, "HNYMASTR",                                       ~
                       varc, indexed, recsize = 900,                     ~
                       keypos = 1, keylen = 25,                          ~
                       alt key  1, keypos = 102, keylen = 9, dup,        ~
                           key  2, keypos =  90, keylen = 4, dup

            select #4, "JBMASTR2",                                       ~
                       varc, indexed, recsize = 1300,                    ~
                       keypos = 1, keylen = 8

            select #5, "JBMATER2",                                       ~
                       varc, indexed, recsize = 400,                     ~
                       keypos = 1, keylen = 22,                          ~
                       alt key  1, keypos = 23, keylen = 48

            select #6, "JBVALUE2",                                       ~
                       varc, indexed, recsize = 300,                     ~
                       keypos = 1, keylen = 23


            select #7, "JBCREDIT",                                       ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 22,                          ~
                       alt key  1, keypos = 23, keylen = 48

            select #8,  "JBSTATUS",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 12,                         ~
                        alt key  1, keypos =  21, keylen = 44,           ~
                            key  2, keypos =  29, keylen = 36

            select #9,  "CDAAUDJI",                                      ~
                        varc, indexed,  recsize =  256,                  ~
                        keypos = 11, keylen =  19,                       ~
                        alt key   1, keypos =   3,  keylen = 27,         ~
                            key   2, keypos =   1,  keylen = 29

            select #10, "RTEMASTR",                                      ~
                         varc, indexed, recsize = 400,                   ~
                         keypos = 5, keylen = 31,                        ~
                         alt key  1, keypos =  1, keylen = 35

            select #11, "WCMASTR",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 5,                          ~
                        alt key  1, keypos = 1, keylen = 6

            select #12, "JBMASTRC",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

            select #13, "VBKLINES"                                       ~
                        varc, indexed, recsize = 700,                    ~
                        keypos = 1, keylen = 28,                         ~
                        alt key  1, keypos = 333, keylen =  20, dup

            select #14, "RCVLINES",                                      ~
                        varc, indexed, recsize = 800,                    ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #15, "VBKMASTR",                                      ~
                        varc, indexed, recsize = 1030,                   ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #16, "PAYLINES",                                      ~
                        varc, indexed, recsize = 541,                    ~
                        keypos = 36, keylen = 28,                        ~
                        alt key 1, keypos =  1, keylen = 63,             ~
                            key 2, keypos = 17, keylen = 47

            select #17, "PAYMASTR",                                      ~
                        varc, indexed, recsize = 350,                    ~
                        keypos = 1, keylen = 25

            select #18, "BOMMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 26, keylen = 31,                        ~
                        alt key   1, keypos =  1, keylen = 56

            select #19, "ENGMASTR",                                      ~
                        varc, indexed, recsize = 2015,                   ~
                        keypos = 1, keylen = 29

            select #20, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #23, "WCOUT",                                         ~
                        varc, indexed, recsize = 68,                     ~
                        keypos = 9, keylen = 23,                         ~
                        alt key  1, keypos = 1, keylen = 27

            select #34, "PIPOUT",                                        ~
                        varc, indexed, recsize = 64,                     ~
                        keypos = 1, keylen = 56,                         ~
                        alt key  1, keypos = 20, keylen = 37

            select #35, "PIPCROSS",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 1, keylen = 71,                         ~
                        alt key  1, keypos = 20, keylen = 52,            ~
                            key  2, keypos = 39, keylen = 33

            select #45, "JBCROSS2",                                      ~
                        varc, indexed, recsize = 94,                     ~
                        keypos = 29, keylen = 19,                        ~
                        alt key   1, keypos =  1, keylen = 47,           ~
                            key   2, keypos = 48, keylen = 47

            select #54, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20

            select #55, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 25,                         ~
                        alt key 1, keypos = 1, keylen = 26

            select #56, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1, keylen = 25

            select #57, "HNYDETAL",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 1, keylen = 42,                         ~
                        alt key 1, keypos = 43, keylen = 6, dup,         ~
                            key 2, keypos = 49, keylen = 2, dup

            select #58, "PIPIN",                                         ~
                        varc, indexed, recsize = 60,                     ~
                        keypos = 30, keylen = 19,                        ~
                        alt key 1, keypos = 1, keylen = 48

            select #59, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos = 1, keylen = 2


            select #60, "HNYQUAN",                                       ~
                        varc, indexed, recsize = 650,                    ~
                        keypos = 17, keylen = 44,                        ~
                        alt key 1, keypos = 17, keylen = 44

            select #61, "HNYPOOL",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 38

        call "SHOSTAT" ("Linking to Shop Floor Review Functions")
            call "OPENCHCK" (# 1, 0%, f2%( 1%), 0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3%), 0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4%), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5%), 0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6%), 0%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 7%), 0%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8%), 0%, " ")
            call "OPENCHCK" (# 9, 0%, f2%( 9%), 0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%), 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11%), 0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12%), 0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13%), 0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14%), 0%, " ")
            call "OPENCHCK" (#15, 0%, f2%(15%), 0%, " ")
            call "OPENCHCK" (#16, 0%, f2%(16%), 0%, " ")
            call "OPENCHCK" (#17, 0%, f2%(17%), 0%, " ")
            call "OPENCHCK" (#18, 0%, f2%(18%), 0%, " ")
            call "OPENCHCK" (#19, 0%, f2%(19%), 0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20%), 0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23%), 0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34%), 0%, " ")
            call "OPENCHCK" (#35, 0%, f2%(35%), 0%, " ")
            call "OPENCHCK" (#45, 0%, f2%(45%), 0%, " ")
            call "OPENCHCK" (#54, 0%, f2%(54%), 0%, " ")
            call "OPENCHCK" (#55, 0%, f2%(55%), 0%, " ")
            call "OPENCHCK" (#56, 0%, f2%(56%), 0%, " ")
            call "OPENCHCK" (#57, 0%, f2%(57%), 0%, " ")
            call "OPENCHCK" (#58, 0%, f2%(58%), 0%, " ")
            call "OPENCHCK" (#59, 0%, f2%(59%), 0%, " ")
            call "OPENCHCK" (#60, 0%, f2%(60%), 0%, " ")
            call "OPENCHCK" (#61, 0%, f2%(61%), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            str(line2$,62) = "JBQDSPLY: " & str(cms2v$,,8)
            inpmessage$ = "Enter Job for Display or press return to see"&~
                          " Jobs on file."

            if f2%(9) = 0% then cda% = 1%

*        Set up selection descriptors for Inquiry Functions
            hdr$     = "        Job Inquiry Functions         "
            opt$( 1%) = "( 1) Job Status Summary               "
            opt$( 2%) = "( 2) Material Ledger Display          "
            opt$( 3%) = "( 3) Labor Cost & Time Details        "
            opt$( 4%) = "( 4) Work Center Cost Details         "
            opt$( 5%) = "( 5) Miscellaneous Cost Details       "
            opt$( 6%) = "( 6) Credit Ledger Display            "
            opt$( 7%) = "( 7) Schedule & Routing Display       "
            opt$( 8%) = "( 8) Review The Need For The Job      "
            opt$( 9%) = "( 9) Job Cost Detail Report           "
            opt$(10%) = "(10) Review/Modify CDA JI Transactions"
            opt$(11%) = "(11) Purchase Jobs - PO Details       "
            opt$(12%) = "(12) Job Text                         "
            pfkeys$  = hex(000102030405060708090a0b0c0dff0f10ff)

            if cda%  = 1% then L10000
                opt$(10) = " "
                str(pfkeys$,11,1) = hex(ff)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************


        menu
          accept                                                         ~
            at(01,02),                                                   ~
                "CAELUS MANAGEMENT SYSTEMS - SHOP FLOOR INQUIRIES",      ~
            at(01,67), "Date: ",                                         ~
            at(01,73), fac(hex(8c)), date$,                              ~
            at(02,02), fac(hex(ac)), line2$,                             ~
            at(04,02), fac(hex(94)), errormsg$, ch(79),                  ~
                                                                         ~
            at(05,02), "Job for Display: ",                              ~
            at(05,19), fac(hex(81)), job$,                               ~
            at(05,29), fac(hex(84)), jobdescr$,                          ~
                                                                         ~
            at(07,20), fac(hex(ac)), hdr$    ,                           ~
            at(08,20), fac(hex(84)), opt$( 1%),                          ~
            at(09,20), fac(hex(84)), opt$( 2%),                          ~
            at(10,20), fac(hex(84)), opt$( 3%),                          ~
            at(11,20), fac(hex(84)), opt$( 4%),                          ~
            at(12,20), fac(hex(84)), opt$( 5%),                          ~
            at(13,20), fac(hex(84)), opt$( 6%),                          ~
            at(14,20), fac(hex(84)), opt$( 7%),                          ~
            at(15,20), fac(hex(84)), opt$( 8%),                          ~
            at(16,20), fac(hex(84)), opt$( 9%),                          ~
            at(17,20), fac(hex(84)), opt$(10%),                          ~
            at(18,20), fac(hex(84)), opt$(11%),                          ~
            at(19,20), fac(hex(84)), opt$(12%),                          ~
                                                                         ~
            at(21,02), fac(hex(ac)), inpmessage$,                        ~
            at(22,65), "(13)Instructions",                               ~
            at(23,65), "(15)Print Screen",                               ~
            at(24,65), "(16)Exit Program",                               ~
                keys(pfkeys$), key(keyhit%)


            if keyhit%  = 13% then call "MANUAL" ("JBQDSPLY")
            if keyhit%  = 15% then call "PRNTSCRN"
            if keyhit%  = 16% then L65000

            call "GETCODE" (#4, job$, jobdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then get #4 using L10460, tagnr$, vendor$, po$,~
                                                    poline$, textid$     ~
                           else goto menu  /* No Job has been selected */
L10460:         FMT POS(39), CH(19), POS(108), CH(9), CH(16), CH(3),     ~
                    POS(228), CH(4)

            if vendor$ <> " " or keyhit% <> 11% then L10470
                errormsg$ = "Job is not a Purchase Job, please re-select."
                goto menu

L10470:     errormsg$ = " "
            if keyhit% =  0% then menu
            if keyhit% =  1% then call_jbqjob
            if keyhit% =  2% then call_jbqmatl
            if keyhit% =  3% then call_jbqvalue_labor
            if keyhit% =  4% then call_jbqvalue_wc
            if keyhit% =  5% then call_jbqvalue_misc
            if keyhit% =  6% then call_jbqcredt
            if keyhit% =  7% then call_jbqschdl
            if keyhit% =  8% then call_getdem
            if keyhit% =  9% then call_jbqcdtls
            if keyhit% = 10% then call_cdaqji
            if keyhit% = 11% then call_postatus
            if keyhit% = 12% then call_txtdsply
            goto menu

        call_jbqjob
            call "JBQJOB" (job$, #4, #54, #3, #34, #5, #6, #12)
            goto menu

        call_jbqmatl
            call "JBQMATL" (job$, #54, #3, #5, #34, #4, #6, #55, #56,    ~
            #59, #58, #57, #1, #35, #12)
            goto menu

        call_jbqcredt
            call "JBQCREDT" (job$, #54, #3, #5, #7, #4, #6, #34, #60,    ~
                             #61, #12)
            goto menu

        call_jbqvalue_labor  :  type$ = "L"  : goto call_jbqvalue
        call_jbqvalue_wc     :  type$ = "W"  : goto call_jbqvalue
        call_jbqvalue_misc   :  type$ = "M"
        call_jbqvalue
            call "JBQVALUE" (job$, type$, #54, #4, #6, #5, #3)
            goto menu

        call_jbqschdl
            call "JBQSCHDL" (job$, #54, #3, #18, #19, #10, #11, #23, #4, ~
                                   #8, #45, #5, #6)
            goto menu

        call_jbqcdtls
            call "JBQCDTLS" (job$, #54, #4, #5, #6, #3, #12)
            goto menu

        call_getdem
            call "GETDEM" (1%, tagnr$, #35, #1, #58, dem$, demtype$, ret%)
            ret% = ret%
            goto menu

        call_cdaqji
            call "CDAQJI" (job$)
            goto menu

        call_postatus
            call "POSTATUS" (vendor$, po$, poline$, 1%, #13, #14, #15,   ~
                             #16, #17, #20)
            goto menu

        call_txtdsply
            call "TXTDSPLY" (#20, f2%(20%), "021", line2$, textid$,      ~
                                                                 text$())
            goto menu

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Gets us outta here! (So long Doogan [HES]).               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
            call "SHOSTAT" (" One Moment Please ")
            end
