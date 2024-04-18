        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BRRR   W   W   CCC    222   W   W   CCC           *~
            *    J    B   B  W   W  C   C  2   2  W   W  C   C          *~
            *    J    BBBB   W W W  C         2   W W W  C              *~
            *  J J    B   B  WW WW  C   C   2     WW WW  C   C          *~
            *   J     BBBB   W   W   CCC   22222  W   W   CCC           *~
            *                                                    center *~
            *-----------------------------------------------------------*~
            * JBWC2WC  - Report actual movement of job part from work   *~
            *            center to work center.  Also allows entry of   *~
            *            actual set up and run times, and who moved the *~
            *            stuff.                                         *~
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
            * 11/14/85 ! ORIGINAL (Re-write)                      ! HES *~
            * 06/17/87 ! Standard Costing Changes                 ! ERN *~
            * 06/21/88 ! Changed file on #6 to GENCODES           ! MJB *~
            * 06/30/88 ! Changed Parameter list for JBACTSUB      ! RJM *~
            * 05/17/93 ! Added DIM statement for parameters passed! WPH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            injob$8,                          /* OPTIONAL JOB NUMBER   */~
            inemp$12,                         /* OPTIONAL EMPLOYEE NO. */~
            indate$8,                         /* OPTIONAL TRAN DATE    */~
            intime$8                          /* OPTIONAL TRAN TIME    */





        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! JBMASTR2 ! LEVEL 2 JOB MASTER FILE                  *~
            * # 2 ! JBSTATUS ! JOB STATUS TRACKING FILE                 *~
            * # 3 ! HNYMASTR ! INVENTORY MASTER (DESCRIPTIONS)          *~
            * # 4 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * # 5 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            * # 6 ! GENCODES ! General Codes File                       *~
            * # 7 ! RTEMASTR ! PRODUCTION ROUTINGS FILE                 *~
            * # 8 ! JBCROSS2 ! Additional Job Info                      *~
            * # 9 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #10 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************

            select #1,  "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8

            select #2,  "JBSTATUS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 12,                         ~
                        alt key 1, keypos =  21, keylen = 44,            ~
                            key 2, keypos =  29, keylen = 36

            select #3,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25

            select #4,  "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2, keylen = 5,                          ~
                        alt key 1, keypos =  1 , keylen = 6

            select #5,  "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #7,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos =  5, keylen =  31,                       ~
                        alt key  1, keypos = 1, keylen = 35

            select #8,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  94,                                   ~
                        keypos =29, keylen = 19,                         ~
                        alternate key 1, keypos = 1 , keylen = 47,       ~
                                  key 2, keypos = 48, keylen = 47

            select #9,  "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos =  26, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 56

            select #10, "ENGMASTR" ,                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #11, "SYSFILE2" ,                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            injob$, inemp$, indate$, intime$ = " "

            call "JBACTSUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,         ~
                             injob$, inemp$, indate$, intime$, 0)

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

            call "SHOSTAT" ("Releasing Files, One Moment Please")

            end
