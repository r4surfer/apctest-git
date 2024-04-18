        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   H   H  N   N  Y   Y   222   JJJJJ  BBBB    *~
            *    J    B   B  H   H  NN  N  Y   Y      2    J    B   B   *~
            *    J    BBBB   HHHHH  N N N   YYY    222     J    BBBB    *~
            *  J J    B   B  H   H  N  NN    Y    2      J J    B   B   *~
            *   J     BBBB   H   H  N   N    Y    22222   J     BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBHNY2JB - Allows returning of parts previously reported  *~
            *            as completed to a Job (DRIVER for JBHNYJBS).   *~
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
            * 07/14/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim f2%(32),                     /* = 0 if the file is open    */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
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
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #5  ! JBMASTR2 ! Production job master file               *~
            * #6  ! JBCREDIT ! Production job credits received detail f *~
            * #7  ! USERINFO ! Users Default Information File           *~
            * #10 ! SERTIF   ! TIF for Serial Numbers involved in Inven *~
            * #11 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #12 ! SERWORK  ! Common Work File used in Handling Serial *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 3, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select # 4, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 5, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8

            select # 6, "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48

            select # 7, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3

            select #10, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  62

            select #11, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #12, "SERWORK",                                       ~
                        varc,     indexed,  recsize = 48,                ~
                        keypos = 1,    keylen = 23

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))

        REM *************************************************************~
            *            I N V O K E   S U B R O U T I N E              *~
            *-----------------------------------------------------------*~
            * Call the subroutine JBHNYJBS.                             *~
            *************************************************************


            call "JBHNYJBS" (" ",                  /* Default Job Nmbr */~
                             "03", "MPC",          /* Module, Journal  */~
                             #2 ,                  /* SYSFILE2         */~
                             #3 ,                  /* HNYMASTR         */~
                             #4 ,                  /* HNYQUAN          */~
                             #5 ,                  /* JBMASTR2         */~
                             #6 ,                  /* JBCREDIT         */~
                             #7 ,                  /* USERINFO         */~
                             #10,                  /* SERTIF           */~
                             #11,                  /* SERMASTR         */~
                             #12 )                 /* SERWORK          */

            end
