        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP    OOO   V   V   SSS    AAA   RRRR   EEEEE  L       *~
            *  P   P  O   O  V   V  S      A   A  R   R  E      L       *~
            *  PPPP   O   O  V   V   SSS   AAAAA  RRRR   EEEE   L       *~
            *  P      O   O   V V       S  A   A  R   R  E      L       *~
            *  P       OOO     V     SSS   A   A  R   R  EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * POVSAREL - Driver Program For POVRELSB - Create Purchase  *~
            *            Directives From Vendor Service Advices.        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1994, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/27/94 ! ORIGINAL                                 ! LDJ *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL


        dim f2%(16),                     /* = 0 if the file is open    */~
            fs%(16),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(16)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PORLSE   ! Purchase Order Requisitions file         *~
            * #03 ! VENDOR   ! Vendor Master Record                     *~
            * #06 ! CURMASTR ! Currency Master file                     *~
            * #07 ! GENCODES ! System General Codes file.               *~
            * #09 ! JBMASTR2 ! Production job master file               *~
            * #10 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #11 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #12 ! HNYMASTR ! Inventory Master File                    *~
            * #13 ! HNYPROC  ! Inventory Procurement History File       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "PORLSE",                                        ~
                        varc,     indexed,  recsize =  492,              ~
                        keypos =    1, keylen =  66,                     ~
                        alt key  5, keypos =  242, keylen =  19, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  1, keypos =   48, keylen =  19, dup

            select #03, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #06, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   4

            select #07, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #10, "VPCMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =   29, keylen =  25,         ~
                            key  2, keypos =   60, keylen =  26, dup

            select #11, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34

            select #12, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #13, "HNYPROC",                                       ~
                        varc,     indexed,  recsize =  134,              ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 1%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))

            call "POVRELSB" (#1,#2,#3,#6,#7,#10,#11,#12,#13)

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1994, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
