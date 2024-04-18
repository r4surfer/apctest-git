        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  RRRR   EEEEE  V   V  U   U  EEEEE   *~
            *  P   P  L      NN  N  R   R  E      V   V  U   U  E       *~
            *  PPPP   L      N N N  RRRR   EEE    V   V  U   U  EEEE    *~
            *  P      L      N  NN  R   R  E       V V   U   U  E       *~
            *  P      LLLLL  N   N  R   R  EEEEE    V     UUU   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNREVUE - DRIVER PROGRAM FOR PLNRSUB, THE PLANNING       *~
            *            REVIEW FUNCTIONS.  ONLY STOP IS A SCREEN TO SET*~
            *            INITIAL AREA OF INTEREST.                      *~
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
            * 11/23/83 ! ORIGINAL                                 ! KEN *~
            * 05/14/87 ! Standard Costing Changes.                ! ERN *~
            * 11/04/88 ! Added HNYALTRS for subroutine            ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim reviewpart$25

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R5.01.03 11/15/88 Patch Release                   "
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
            * # 1 ! DEMMASTR ! Demand Master File                       *~
            * # 2 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! HNYDETAL ! MOVEMENT DETAILS                         *~
            * # 7 ! RTEMASTR ! Production routing master file           *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! CALMASTR ! Planning Production Calendar File        *~
            * #15 ! BOMMASTR ! BOM relationship file                    *~
            * #16 ! HNYALTRS ! Parts Alternates File                    *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            * #24 ! ENGMASTR ! Engineering Master Filer                 *~
            * #33 ! PIPIN    ! Planned inventory additions detail       *~
            * #34 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #40 ! SFMASTR2 ! Sales forecast master file               *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select # 2, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 4, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #5, "HNYDETAL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select # 6, "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  34

            select # 7, "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #12, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #15, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #16, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #23, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #24, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #33, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #35, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   150,                                 ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #40, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #41, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#23, "SHARE", f2%(23), rslt$(23), axd$(23))
            call "OPENFILE" (#24, "SHARE", f2%(24), rslt$(24), axd$(24))
            call "OPENFILE" (#33, "SHARE", f2%(33), rslt$(33), axd$(33))
            call "OPENFILE" (#34, "SHARE", f2%(34), rslt$(34), axd$(34))
            call "OPENFILE" (#35, "SHARE", f2%(35), rslt$(35), axd$(35))
            call "OPENFILE" (#40, "SHARE", f2%(40), rslt$(40), axd$(40))
            call "OPENFILE" (#41, "SHARE", f2%(41), rslt$(41), axd$(41))

        first_screen

        accept                                                           ~
               at (01,05),                                               ~
        "THESE FUNCTIONS ARE AVAILABLE TO HELP REVIEW CURRENT PLANS AND A~
        ~CTIVITIES",                                                      ~
                                                                         ~
               at (03,07),                                               ~
        "+---------+-----------------------------------------------------~
        ~----+",                                                          ~
               at (04,07),                                               ~
        "! (ENTER) !                                                     ~
        ~    !",                                                          ~
               at (05,07),                                               ~
        "!  PF( 1) ! See Planned Inventory Position And Available To Comm~
        ~it  !",                                                          ~
               at (06,07),                                               ~
        "!  PF( 2) ! See Inventory Sources And Applications              ~
        ~    !",                                                          ~
               at (07,07),                                               ~
        "!  PF( 3) ! See Inventory Sales Versus Forecasts                ~
        ~    !",                                                          ~
               at (08,07),                                               ~
        "+---------+-----------------------------------------------------~
        ~----+",                                                          ~
               at (09,07),                                               ~
        "!  PF( 4) ! Examine Planned Inventory Position Details          ~
        ~    !",                                                          ~
               at (10,07),                                               ~
        "!    (  ) !                                                     ~
        ~    !",                                                          ~
               at (11,07),                                               ~
        "!  PF( 6) ! Examine Work Center Capacities                      ~
        ~    !",                                                          ~
               at (12,07),                                               ~
        "+---------+-----------------------------------------------------~
        ~----+",                                                          ~
               at (13,07),                                               ~
        "!  PF( 7) ! Review Effective Dates                              ~
        ~    !",                                                          ~
               at (14,07),                                               ~
        "!  PF( 8) ! Review Bill Of Materials                            ~
        ~    !",                                                          ~
               at (15,07),                                               ~
        "!  PF( 9) ! Review Work Center Routings                         ~
        ~    !",                                                          ~
               at (16,07),                                               ~
        "+---------+-----------------------------------------------------~
        ~----+",                                                          ~
               at (17,07),                                               ~
        "!  PF(10) ! Examine Current Demand Status                       ~
        ~    !",                                                          ~
               at (18,07),                                               ~
        "!    (  ) !                                                     ~
        ~    !",                                                          ~
               at (19,07),                                               ~
        "!    (  ) !                                                     ~
        ~    !",                                                          ~
               at (20,07),                                               ~
        "!    (  ) !                                                     ~
        ~    !",                                                          ~
               at (21,07),                                               ~
        "!    (  ) !                                                     ~
        ~    !",                                                          ~
               at (22,07),                                               ~
        "+----+----+---------------------------------------------------+-~
        ~----+",                                                          ~
               at (23,12),                                               ~
             "! (13)INSTRUCTIONS   (15)PRINT SCREEN   (16)EXIT PROGRAM !"~
              ,at (24,12),                                               ~
             "+--------------------------------------------------------+"~
                                                                         ~
            ,keys(hex(000102030405060708090a0b0c0d0e0f10)), key(keyhit%)

            if keyhit%=16% then L65000

            if keyhit%<>15% then L10810
                call "PRNTSCRN"
                goto first_screen

L10810:     if keyhit%<>13% then L10850
                call "MANUAL" ("PLNRSUB")
                goto first_screen

L10850:     mode%=0%

            if keyhit%= 0% then mode%= 1%
            if keyhit%= 1% then mode%= 1%
            if keyhit%= 2% then mode%= 2%
            if keyhit%= 3% then mode%= 5%
            if keyhit%= 4% then mode%= 6%
            if keyhit%= 6% then mode%= 8%
            if keyhit%= 7% then mode%= 4%
            if keyhit%= 8% then mode%= 9%
            if keyhit%= 9% then mode%= 3%
            if keyhit%=10% then mode%= 7%

            if mode%=0% then first_screen

            call "PLNRSUB" (mode%,reviewpart$,                           ~
                                        #1,#2,#4,#7,#11,#12,#15,#23,     ~
                                        #24,#33,#34,#35,#40,#41,#5,#6,#16)

            goto first_screen

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

            call "SHOSTAT" ("One Moment Please.")

            end
