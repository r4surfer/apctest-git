        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    OOO   RRRR   EEEEE  L      EEEEE   AAA    SSS    *~
            *  P   P  O   O  R   R  E      L      E      A   A  S       *~
            *  PPPP   O   O  RRRR   EEEE   L      EEEE   AAAAA   SSS    *~
            *  P      O   O  R   R  E      L      E      A   A      S   *~
            *  P       OOO   R   R  EEEEE  LLLLL  EEEEE  A   A   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PORELEAS - Release Purchase Advices by Part or Release    *~
            *            Purchase Advices by Service Order,creating 99% *~
            *            of a VBKLINE to feed into VBKINPUT. The pur-   *~
            *            chase agent will make critical descisions such *~
            *            as which vendor, what to pay, etc.             *~
            *            (Actually this is a driver which branches to   *~
            *            either PORELSUB (PARTS) or POVRELSB (SERVICE)).*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/03/84 ! ORIGINAL                                 ! KEN *~
            * 12/13/85 ! File format changes                      ! MJB *~
            * 04/14/87 ! Standard Costing Changes                 ! ERN *~
            * 01/12/93 ! Purchase Job-Added 5th Alt Key to PORLSE ! JBK *~
            * 07/12/94 ! Changed calling syntax to PORELSUB.      ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim f2%(16),                     /* = 0 If the file is open    */~
            fs%(16),                     /* = 1 if File Open           */~
            rslt$(16)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! PIPIN    ! Planned inventory additions detail       *~
            * # 2 ! PORLSE   ! release control buffer                   *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * # 6 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 7 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 8 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            * # 9 ! HNYPROC  ! Inventory Procurement History File       *~
            * #10 ! HNYGENER ! Inventory Generic Parts Cross-Reference  *~
            * #11 ! PIPCROSS ! Planned Inventory Position Tag Xref File *~
            * #12 ! POADVMAS ! Purchase Advices (Supplemental) Master   *~
            * #13 ! GENCODES ! General Codes File                       *~
            * #14 ! VPCMASTR ! Vendor Purchase Contract Master File     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select # 2, "PORLSE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos =   1, keylen =  66,                      ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup,    ~
                            key  5, keypos =  242, keylen =  19, dup

            select # 3, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select # 4, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 5, "VENPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   256,                                 ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select # 6, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select # 7, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =   2, keylen =  25,                      ~
                        alt key  1, keypos =    1, keylen =  26

            select # 8, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =   1, keylen =  25

            select #9, "HNYPROC",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select #10, "HNYGENER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41          ~

            select #11, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   1, keylen =  71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #12, "POADVMAS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =   35, keylen =  19,                     ~
                        alt key  1, keypos =   10, keylen =  44,         ~
                            key  2, keypos =    1, keylen =  53,         ~
                            key  3, keypos =   62, keylen =   8, dup

            select #13, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #14, "VPCMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 10, keylen = 20,                        ~
                        alt key  1, keypos =   1,  keylen =  29,         ~
                            key  2, keypos =  60,  keylen =  26, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(1%), rslt$(4%), rslt$(6%) = "REQUIRED"
            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 1%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            if f2%(1%)=1% or f2%(4%)=1% or f2%(6%)=1% then exit_program

            call "PORELSUB" (#1, #2, #3, #4, #5, #6, #7, #8, #9, #10,    ~
                                     #11, #12, #13, #14, f2%(2%))

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
