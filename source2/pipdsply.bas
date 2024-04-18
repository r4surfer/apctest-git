        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   DDDD    SSS   PPPP   L      Y   Y   *~
            *  P   P    I    P   P  D   D  S      P   P  L      Y   Y   *~
            *  PPPP     I    PPPP   D   D   SSS   PPPP   L       YYY    *~
            *  P        I    P      D   D      S  P      L        Y     *~
            *  P      IIIII  P      DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPDSPLY - DISPLAY AND/OR PRINT PROGRAM FOR THE THREE     *~
            *            PIP (PLANNED INVENTORY POSITION) FILES.        *~
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
            * 04/02/84 ! ORIGINAL                                 ! JRW *~
            * 04/20/87 ! Minor Cleanup                            ! MJB *~
            * 05/27/87 ! File changes for standard costing        ! MJB *~
            * 11/04/88 ! Added HNYALTRS for PLNRSUB               ! KAB *~
            * 11/14/88 ! Added RTEALTRS for PLNRSUB               ! KAB *~
            * 10/25/93 ! Purchase Job Project - Added 5th Alt Key ! JBK *~
            *          !   to PORLSE                              !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! PIPIN    ! Planned inventory additions detail  feed *~
            * #3  ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * #4  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #5  ! USERINFO ! Users Default Information File           *~
            * #6  ! HNYMASTR ! Inventory Master File                    *~
            * #7  ! RTEMASTR ! Production routing master file           *~
            * #8  ! JBMASTR2 ! Production Job Master File               *~
            * #9  ! PORLSE   ! Purchase Directive (Requisition) File    *~
            * #10 ! VBKMASTR ! Purchase Order Header File               *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! CALMASTR ! Planning Production Calendar File        *~
            * #15 ! BOMMASTR ! BOM relationship file                    *~
            * #16 ! HNYALTRS ! Alternate Parts File                     *~
            * #17 ! RTEALTRS ! Alternate Routes File                    *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            * #24 ! ENGMASTR ! Engineering Master Filer                 *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #40 ! SFMASTR2 ! Sales forecast master file               *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #45 ! HNYDETAL ! MOVEMENT DETAILS                         *~
            * #46 ! DEMMASTR ! Demand Master File                       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20

            select #2,  "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #3,  "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #4,  "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #5,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3

            select #6,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #7,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos = 5, keylen =  31,                        ~
                        alt key  1, keypos =   1, keylen =  35

            select  #8, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8

            select #10, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #9,  "PORLSE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos =   1, keylen =  66,                      ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup,    ~
                            key  5, keypos =  242, keylen =  19, dup

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #12, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2

            select #15, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #16, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #17, "RTEALTRS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =   1, keylen = 34

            select #23, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

            select #24, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29

            select #35, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #40, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25

            select #41, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            select #45, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select #46, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#4,  "SHARE", f2%(4 ), rslt$(4 ), axd$(4 ))
            call "OPENFILE" (#5,  "SHARE", f2%(5 ), rslt$(5 ), axd$(5 ))
            call "OPENFILE" (#6,  "SHARE", f2%(6 ), rslt$(6 ), axd$(6 ))
            call "OPENFILE" (#7,  "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))
            call "OPENFILE" (#8,  "SHARE", f2%(8 ), rslt$(8 ), axd$(8 ))
            call "OPENFILE" (#9,  "SHARE", f2%(9 ), rslt$(9 ), axd$(9 ))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
            call "OPENFILE" (#23, "SHARE", f2%(23), rslt$(23), axd$(23))
            call "OPENFILE" (#24, "SHARE", f2%(24), rslt$(24), axd$(24))
            call "OPENFILE" (#35, "SHARE", f2%(35), rslt$(35), axd$(35))
            call "OPENFILE" (#40, "SHARE", f2%(40), rslt$(40), axd$(40))
            call "OPENFILE" (#41, "SHARE", f2%(41), rslt$(41), axd$(41))
            call "OPENFILE" (#45, "SHARE", f2%(45), rslt$(45), axd$(45))
            call "OPENFILE" (#46, "SHARE", f2%(46), rslt$(46), axd$(46))

        REM And NOW, The Actual Subroutine Call!

            call "PIPDSUB" (#1, #2, #3, #4, #6, #46, #45, #7, #11, #12,  ~
                     #15, #23, #24, #35, #40, #41, #8, #9, #10, #17, #16)

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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
