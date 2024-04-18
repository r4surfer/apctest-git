        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP    OOO    AAA   N   N   AAA   L       *~
            *  P   P    I    P   P  O   O  A   A  NN  N  A   A  L       *~
            *  PPPP     I    PPPP   O   O  AAAAA  N N N  AAAAA  L       *~
            *  P        I    P      O   O  A   A  N  NN  A   A  L       *~
            *  P      IIIII  P       OOO   A   A  N   N  A   A  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPOANAL - ALLOWS USER TO REPORT ON PIP OUT'S USING A     *~
            *            RANGE OF CRITERIA.  PIPOUT'S INCLUDE SALES TO  *~
            *            BE SHIPPED, PARTS TO BE KITTED, PURCHASES FOR  *~
            *            SPECIFIC JOBS/PROJECTS.                        *~
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
            * 01/03/84 ! ORIGINAL                                 ! BLT *~
            * 12/26/85 ! File changes- VBKMASTR, VBKLINES,        ! ERN *~
            *          !  VBKBUFFR, VBKBUF2                       !     *~
            *          !  Also to V2PMASTR/LINES                  !     *~
            * 11/05/86 ! File changes- BCKMASTR, BCKLINES         ! ERN *~
            * 07/29/87 ! Std Costing Enhancements, Misc clean-up. ! ERN *~
            * 03/11/88 ! Fixed Bug - Endless Loop if No Part Found! MDE *~
            * 03/17/88 ! Made A Mindless Driver For PIPOASUB      ! MDE *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
            dim f2%(64),                 /* 0 IF FILE IS OPEN */         ~
                rslt$(64)20,             /* TEXT FROM FILE OPEN */       ~
                axd$(64)4                /* ALT KEY PIOTNER FROM OPEN */ ~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00       "
        REM *************************************************************
            mat f2% = con

        REM  *********************************************************** ~
             *           S E L E C T   F I L E S                       * ~
             * ------------------------------------------------------- * ~
             * FILE ! PRNAME ! D E S C R I P T I O N                  !* ~
             * -----+--------+----------------------------------------+  ~
             * # 1  !USERINFO! User Default Information File          !  ~
             * # 8  !HNYMASTR! Inventory Master                       !  ~
             * #11  !VBKMASTR! PO Master File                         !  ~
             * #12  !VBKLINES! PO Line Item File                      !  ~
             ******************************************************** !  ~
        REM *       FILE SELECTION AND OPEN CALLS                       *


            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3                      ~


            select # 11, "VBKMASTR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos  =  1, keylen =  25,                      ~
                        alt key    1, keypos =  10,  keylen =  16

            select #12, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos  =    1, keylen =  28

            select # 6, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~



            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, "SHARE", f2%( 1),axd$(1), rslt$( 1))
            call "OPENCHCK" (# 6, "SHARE", f2%( 6),axd$(6), rslt$( 6))
            call "OPENCHCK" (#11, "SHARE", f2%(11),axd$(11), rslt$(11))
            call "OPENCHCK" (#12, "SHARE", f2%(12),axd$(12), rslt$(12))

            call "PIPOASUB" (" ",#1,#6,#11,#12)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment please")

            call "SETPRNT" ("PLN001", " ", 0%, 1%)

            end
