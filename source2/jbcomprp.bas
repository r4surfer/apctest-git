        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    CCC    OOO   M   M  PPPP   RRRR   PPPP    *~
            *    J    B   B  C   C  O   O  MM MM  P   P  R   R  P   P   *~
            *    J    BBBB   C      O   O  M M M  PPPP   RRRR   PPPP    *~
            *  J J    B   B  C   C  O   O  M   M  P      R  R   P       *~
            *   J     BBBB    CCC    OOO   M   M  P      R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCOMPRP - Reports completion of all or part of a job and *~
            *            handles attendant G/L and Inventory posting    *~
            *            (including Lot Tracking, Serial # Tracking, etc*~
            *            if applicable).                                *~
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
            * 11/02/83 ! ORIGINAL                                 ! KEN *~
            * 02/02/84 ! JBCMPSUB USES USERS HNYDATE              ! KAB *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL CHANGES            ! RAC *~
            * 10/10/85 ! Posting moved to background, changed call! HES *~
            * 02/19/87 ! Changes for Serial Number Tracking, ...  ! LDJ *~
            * 05/19/93 ! More Channels to JBCMPSUB (Why Else)     ! KAB *~
            * 08/13/93 ! Added #16 WCOUT for call to JBCMPSUB.    ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

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
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! JBMASTR2 ! Production job master file               *~
            * # 5 ! JBMATER2 ! Job Materials Ledger                     *~
            * # 6 ! JBVALUE2 ! Job Value Added Ledger                   *~
            * # 7 ! JBCREDIT ! Production job credits received detail f *~
            * # 8 ! JBSTATUS ! Job status tracking file                 *~
            * # 9 ! JBMASTRC ! JBMASTR2 Core Appendix                   *~
            * #10 ! RTEMASTR ! Standard & alt work center routings      *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! GLMAIN   ! G/L Account Master File                  *~
            * #16 ! WCOUT    ! Work center useage cross reference       *~
            * #20 ! USERINFO ! User Posting Dates file                  *~
            * #34 ! PIPOUT   ! PIP Quantities due out                   *~
            * #45 ! JBCROSS2 ! Job RTE/BOM used cross ref.              *~
            * #52 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #54 ! SYSFILE2 ! Caelus Management System Information     *~
            * #59 ! STORNAME ! Table Of Valid Store Numbers             *~
            * #61 ! SERTIF   ! Serial Numbers Transaction Image File    *~
            * #62 ! SERMASTR ! Serial Numbers Master File               *~
            * #63 ! SERWORK  ! Serial Numbers Work File                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 3, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 4, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos =    1, keylen =   8                      ~

            select #5, "JBMATER2",                                       ~
                       varc, indexed, recsize = 400,                     ~
                       keypos = 1, keylen = 22,                          ~
                       alt key  1, keypos = 23, keylen = 48

            select #6, "JBVALUE2",                                       ~
                       varc, indexed, recsize = 300,                     ~
                       keypos = 1, keylen = 23

            select # 7, "JBCREDIT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #8,  "JBSTATUS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 12,                          ~
                        alt key 1, keypos =  21, keylen = 44,            ~
                            key 2, keypos =  29, keylen = 36

            select #9,  "JBMASTRC",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos= 1, keylen =  8

            select #10, "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos =   5, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 35

           select #11,  "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =2024,                                   ~
                        keypos =   2 , keylen = 5,                       ~
                        alt key 1, keypos =  1 , keylen = 6

            select #12, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  9

            select #16, "WCOUT",                                         ~
                        varc, indexed, recsize =  68,                    ~
                        keypos = 9, keylen = 23,                         ~
                        alt key  1, keypos =  1, keylen = 27

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 64,                                    ~
                        keypos =  1 , keylen =  56,                      ~
                        alt key 1, keypos = 20, keylen = 37

            select #45,"JBCROSS2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  94,                                   ~
                        keypos =29, keylen = 19,                         ~
                        alternate key 1, keypos = 1 , keylen = 47,       ~
                                  key 2, keypos = 48, keylen = 47

            select #52, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44         ~

            select #54, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #59, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #61, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #63, "SERWORK",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos = 1, keylen = 23


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENCHCK" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))
            call "OPENFILE" (#34, "SHARE", f2%(34), rslt$(34), axd$(34))
            call "OPENFILE" (#45, "SHARE", f2%(45), rslt$(45), axd$(45))
            call "OPENFILE" (#52, "SHARE", f2%(52), rslt$(52), axd$(52))
            call "OPENFILE" (#54, "SHARE", f2%(54), rslt$(54), axd$(54))
            call "OPENFILE" (#59, "SHARE", f2%(59), rslt$(59), axd$(59))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Just a Stub Driver to JBCMPSUB.                           *~
            *************************************************************

            call "JBCMPSUB" ("03", "MPC", #3, #4, #7, #8, #9, #10, #11,  ~
                             #16, #45, #52, #54, #59, #20, #34, #12, #5, ~
                             #6, #61, #62, #63, " ", " ")

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
