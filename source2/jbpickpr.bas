        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP   IIIII   CCC   K   K  PPPP   RRRR    *~
            *    J    B   B  P   P    I    C   C  K  K   P   P  R   R   *~
            *    J    BBBB   PPPP     I    C      KKK    PPPP   RRRR    *~
            *  J J    B   B  P        I    C   C  K  K   P      R   R   *~
            *   J     BBBB   P      IIIII   CCC   K   K  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPICKPR - Call subroutine "JBPICKCR" Providing a range of*~
            *            Jobs.  JBPICKCR Then calls  JBPICKSL For Pick  *~
            *            Lists - JBPICKBP for By-Products Lists and     *~
            *            JBTRAVEL for Travelers.                        *~
            *----------------------------------------------------------Z*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/07/83 ! ORIGINAL                                 ! KEN *~
            * 07/08/83 ! PRINT PICK SLIPS FOR PROJECTS & JOBS     ! JRW *~
            * 08/22/84 ! PRINT TRAVEL ALONE OR W/PICK SLIP        ! DSH *~
            * 10/01/86 ! PRINT By-Products List                   ! HDC *~
            * 06/15/87 ! HNYMASTR, JBMASTR2, RTEMASTR file lengths! JIM *~
            * 01/03/89 ! Removed JOBMASTR file from program and   ! MJB *~
            *          !  from argument list for JBPICKCR         !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R5.01.07 09/07/89 Patch Release R5.01.07          "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            *  1  ! SYSFILE2 ! SYSTEM CATCH ALL FILE                    *~
            *  2  ! BOMMASTR ! BILLS OF MATERIALS STRUCTURES            *~
            *  3  ! PIPOUT   ! PLANNED POSITION OUT                     *~
            *  4  ! WCOUT    ! WORK CENTER DETAIL FILE                  *~
            *  5  ! HNYMASTR ! INVENTORY MASTER (DESCRIPTIONS)          *~
            *  6  ! JBMASTR2 ! VERSION 2 JOB MASTER FILE                *~
            *  8  ! RTEMASTR ! ROUTE MASTER FILE                        *~
            *  9  ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            *  10 ! JBCROSS2 ! JOB CROSS REFERENCE FILE                 *~
            *  11 ! HNYQUAN  ! INVENTORY QUANTITIES FILE                *~
            *  12 ! ENGMASTR ! BOM AND RTE EFFECTIVITYT DATES           *~
            *  13 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

             select #2, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos =  26, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 56

            select  #3, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos = 1, keylen = 56,                         ~
                        alternate key 1, keypos = 20, keylen = 37

            select  #4, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 68,                                    ~
                        keypos = 9, keylen = 23,                         ~
                        alternate key 1, keypos = 1, keylen = 27

            select #5, "HNYMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 900,                                    ~
                       keypos = 1, keylen = 25,                          ~
                       alternate key 1, keypos = 102, keylen = 9, dup,   ~
                                 key 2, keypos = 90,  keylen = 4, dup

            select #6, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8

            select # 8, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select # 9, "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  2, keylen = 5,                        ~
                         alt key  1, keypos = 1, keylen = 6

            select #10,"JBCROSS2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

           select #11, "HNYQUAN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 34,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select #12, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

           select #13, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENCHCK" (# 1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "JBPICKCR"(#1,#2,#3,#4,#5,#6,#8,#9,#10,#11,#12,#13)


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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
