        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   OOO   H   H  N   N  Y   Y   222    *~
            *    J    B   B    T    O   O  H   H  NN  N  Y   Y  2   2   *~
            *    J    BBBB     T    O   O  HHHHH  N N N   YYY      2    *~
            *  J J    B   B    T    O   O  H   H  N  NN    Y      2     *~
            *   J     BBBB     T     OOO   H   H  N   N    Y    22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTOHNY2 - Stand-alone driver for JBHNYSUB.               *~
            *            Provides the ability to move (return) component*~
            *            parts from a job back into inventory.          *~
            *            (Presumably these parts were not used by the   *~
            *            Job).                                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/27/87 ! Original                                 ! LCJ *~
            * 06/17/87 ! Standard Costing Modifications           ! ERN *~
            * 05/28/93 ! PIPOUT to JBHNYSUB                       ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim f2%(12),                     /* = 0 if the file is open    */~
            fs%(12),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(12)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
            * # 1 ! USERINFO ! Users Default Information File           *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! JBMASTR2 ! Production job master file               *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * # 6 ! JBMATER2 ! Production job material used detail file *~
            * # 7 ! STORNAME ! STORE INFORMATION FILE                   *~
            * # 8 ! SERTIF   ! TIF for Serial Numbers involved in Inven *~
            * # 9 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #10 ! SERWORK  ! Common Work File used in Handling Serial *~
            * #11 ! PIPOUT   ! PIP Withdrawals File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1350,              ~
                        keypos =    1, keylen =   8                      ~

            select # 4, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 5, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 6, "JBMATER2",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select # 7, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select # 8, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  62                      ~

            select # 9, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76          ~

            select #10, "SERWORK",                                       ~
                        varc,     indexed,  recsize = 48,                ~
                        keypos = 1,    keylen = 23                       ~

            select #11, "PIPOUT",                                        ~
                        varc, indexed, recsize = 64,                     ~
                        keypos = 1, keylen = 56,                         ~
                        alt key  1, keypos = 20, keylen = 37

            call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(2) = "REQUIRED"
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

            if fs%(2) < 0% then exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        call "JBHNYSUB" ("03", "MPW",    /* GL Posting directives      */~
                         #1,             /* USERINFO  File UFB         */~
                         #2,             /* SYSFILE2  File UFB         */~
                         #3,             /* JBMASTR2  File UFB         */~
                         #4,             /* HNYMASTR  File UFB         */~
                         #5,             /* HNYQUAN   File UFB         */~
                         #6,             /* JBMATER2  File UFB         */~
                         #7,             /* STORNAME  File UFB         */~
                         #8,             /* SERTIF    File UFB         */~
                         #9,             /* SERMASTR  File UFB         */~
                         #10,            /* SERWORK   File UFB         */~
                         #11,            /* PIPOUT    File UFB         */~
                         " ",            /* Passed In Job (Optional)   */~
                         "RJ",           /* RJ = Return to Inventory   */~
                                         /*      from Job.             */~
                         f2%(2))         /* OPEN Status of SYSFILE2    */

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
