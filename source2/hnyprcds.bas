        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   RRRR    CCC   DDDD    SSS    *~
            *  H   H  NN  N   Y Y   P   P  R   R  C   C  D   D  S       *~
            *  HHHHH  N N N    Y    PPPP   RRRR   C      D   D   SSS    *~
            *  H   H  N  NN    Y    P      R   R  C   C  D   D      S   *~
            *  H   H  N   N    Y    P      R   R   CCC   DDDD    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPRCDS - Dummy Driver program for HNYPRCSB for Stand-   *~
            *            alone running of the subroutine.               *~
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
            * 07/14/83 ! ORIGINAL                                 ! KEN *~
            * 10/04/85 ! Changed VENDOR File Format               ! MJB *~
            * 05/07/86 ! Renamed from PRCDSPLY to HNYPRCDS,       ! LDJ *~
            *          !   Changed calling syntax to subroutine.  !     *~
            * 05/13/87 ! HNYMASTR record length change (whew!)    ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYPROC  ! Inventory where procurred from detail fi *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 4 ! VENPRICE ! Vendor Price Catalogue File              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "HNYPROC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select # 2, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select # 4, "VENPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =   1, keylen =  34, dup,     ~
                            key  2, keypos =  35, keylen =  34           ~


            call "SHOSTAT"  ("Opening Files, One Moment Please...")
            rslt$(1), rslt$(4) = "REQUIRED"
            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))

                if f2%(1)<>0 and f2%(4)<>0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "HNYPRCSB" (" ", " ", 0%, #1, #2, #3, #4)

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please...")
            end
