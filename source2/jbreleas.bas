        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   RRRR   EEEEE  L      EEEEE   AAA    SSS    *~
            *    J    B   B  R   R  E      L      E      A   A  S       *~
            *    J    BBBB   RRRR   EEEE   L      EEEE   AAAAA   SSS    *~
            *  J J    B   B  R   R  E      L      E      A   A      S   *~
            *   J     BBBB   R   R  EEEEE  LLLLL  EEEEE  A   A   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBRELEAS - This program allows release of planned job     *~
            *            orders. When Job order numbers are assigned    *~
            *            PIPIN, PIPOUT, and WCOUT records are updated   *~
            *            accordingly and a JBMASTR2 record is created.  *~
            *            Program was formerly called JBPORELS.          *~
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
            * 04/06/83 ! ORIGINAL                                 ! KEN *~
            * 10/06/83 ! CHANGED INTO SUBROUTINE                  ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 10/20/86 ! CHANGED CALL 2 JBTRAVEL 2 INCLUDE BOM&ENG! HES *~
            * 02/04/87 ! Cleanup of Program Remarks above & below.! LDJ *~
            *          ! Plus added Serial Number File Selects.   !     *~
            * 06/10/87 ! Program name changed to JBRELEAS from    ! JIM *~
            *          !   JBPORELS. HNYMASTR, JBMASTR2, RTEMASTR !     *~
            *          !   record length mods. See also JBRELSUB. !     *~
            * 01/19/90 ! Add an Alt Key to JBMASTR2 selec statment! SID *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        dim f2%(64)                      /* File status flags for      */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! PIPMASTR ! Planned inv. position master             *~
            * #3  ! HNYMASTR ! Inventory master (descriptions)          *~
            * #4  ! JBMASTR2 ! Job master file                          *~
            * #5  ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #6  ! SERMASTR ! Serial Number Tracking Master File       *~
            * #7  ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #8  ! SFCUM2   ! Generally worthless Forecast File        *~
            * #10 ! RTEMASTR ! Standard & alt work center routings      *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! GLMAIN   ! General ledger master file               *~
            * #18 ! BOMMASTR ! Bills of materials structures            *~
            * #19 ! ENGMASTR ! Bom and Rte Effectivity dates            *~
            * #20 ! USERINFO ! User posting dates                       *~
            * #23 ! WCOUT    ! Work center cross reference file         *~
            * #33 ! PIPIN    ! Planned position in                      *~
            * #34 ! PIPOUT   ! Planned position out                     *~
            * #35 ! PIPCROSS ! Hard peg file                            *~
            * #36 ! JBPIPXRF ! Option part pegging                      *~
            * #45 ! JBCROSS2 ! Job rte/bom used cross ref.              *~
            * #52 ! HNYQUAN  ! Inventory quantity  file                 *~
            * #54 ! SYSFILE2 ! Inventory default values                 *~
            * #59 ! STORNAME ! Store codes                              *~
            *************************************************************


            select #2,   "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #3, "HNYMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 900,                                    ~
                       keypos = 1, keylen = 25

            select #4, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8,                           ~
                       alt key  1, keypos = 1120, keylen =  19, dup,     ~
                           key  2, keypos =   58, keylen =  25, dup

            select  #5, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select  #6, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select  #7, "SERWORK",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos = 1, keylen = 23

            select  #8, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos = 1, keylen = 25

            select #10, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

           select #11,  "WCMASTR", varc, indexed,                        ~
                     recsize =2024 , keypos =   2 , keylen = 5,          ~
                     alternate key 1, keypos =  1 , keylen = 6


            select  #12, "GLMAIN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #18, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos =  26, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 56

            select #19, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

           select #23, "WCOUT",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos = 9, keylen = 23,                         ~
                        alt key 1, keypos = 1, keylen = 27

           select #33, "PIPIN"   , varc, indexed,                        ~
                     recsize = 60,   keypos =  30 , keylen =  19,        ~
                     alt key 1, keypos = 1 , keylen = 48

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos = 1, keylen = 56,                         ~
                        alternate key 1, keypos = 20, keylen = 37

            select #35, "PIPCROSS", varc, indexed,                       ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

           select #36, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

            select #45,"JBCROSS2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

            select #52, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select #54, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #59, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 2, 0%, f2%( 2), 0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3), 0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 200%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8), 0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10), 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11), 0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12), 0%, " ")
            call "OPENCHCK" (#18, 0%, f2%(18), 0%, " ")
            call "OPENCHCK" (#19, 0%, f2%(19), 0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20), 0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23), 0%, " ")
            call "OPENCHCK" (#33, 0%, f2%(33), 0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34), 0%, " ")
            call "OPENCHCK" (#35, 0%, f2%(35), 0%, " ")
            call "OPENCHCK" (#36, 0%, f2%(36), 0%, " ")
            call "OPENCHCK" (#45, 0%, f2%(45), 0%, " ")
            call "OPENCHCK" (#52, 0%, f2%(52), 0%, " ")
            call "OPENCHCK" (#54, 0%, f2%(54), 0%, " ")
            call "OPENCHCK" (#59, 0%, f2%(59), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Call sub.                                                 *~
            *************************************************************

            call "JBRELSUB" (#2, #3, #4,#5,#6, #7, #8,#10, #11, #12, #20,~
                    #23, #33,#34, #45, #52, #54, #35, #36, #59, #18, #19,~
                                        f2%())

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Display message and Exit.                                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "SHOSTAT" ("One Moment Please")
            end
