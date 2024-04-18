        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   EEEEE  M   M  RRRR   EEEEE  V   V  U   U  EEEEE   *~
            *  D   D  E      MM MM  R   R  E      V   V  U   U  E       *~
            *  D   D  EEEE   M M M  RRRR   EEEE   V   V  U   U  EEEE    *~
            *  D   D  E      M   M  R   R  E       V V   U   U  E       *~
            *  DDDD   EEEEE  M   M  R   R  EEEEE    V     UUU   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMREVUE - Driver for Subroutine DEMSTAT which displays   *~
            *            the status of all procurements leading up to   *~
            *            the fullfillment of a customers order.         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/29/86 ! Original                                 ! WPH *~
            * 12/04/87 ! DRIVER% so DEMSTAT can tell how called   ! JDH *~
            * 06/06/88 ! Misc cleanup for R5.01                   ! JDH *~
            * 07/02/92 ! Added Select for PIPIN and pass it to    ! WPH *~
            *          ! DEMSTAT.                                 !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            demstring$19                 /* Demand code and line       */

        dim f2%(32),                     /* = 0 if the file is open    */~
            fs%(32)                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #1  ! PIPCROSS ! hard peg cross reference                 *~
            * #2  ! DEMMASTR ! Demand Master File                       *~
            * #3  ! JBMASTR2 ! Production job master file               *~
            * #4  ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #5  ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * #6  ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #7  ! VBKMASTR ! Purchase Order Headers Master File       *~
            * #8  ! VBKLINES ! Purchase Orders Line Item Master file    *~
            * #9  ! JBCREDIT ! Production job credits received detail f *~
            * #10 ! RCVLINES ! Receiver Lines File                      *~
            * #11 ! PAYMASTR ! Payables header file                     *~
            * #12 ! PAYLINES ! Payables lines  file                     *~
            * #13 ! HNYMASTR ! Inventory Master File                    *~
            * #14 ! WCMASTR  ! Workcenter Master File                   *~
            * #15 ! RTEMASTR ! Routings Master File                     *~
            * #16 ! TXTFILE  ! System Text File                         *~
            * #17 ! PIPIN    ! Expected Inventory Additions file        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
        REM *************************************************************

            select #1,  "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #2,  "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #3,  "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #4,  "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select #5,  "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #6,  "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #7,  "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #8,  "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #9,  "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #10, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #11, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize = 350 ,              ~
                        keypos =    1, keylen =  25


            select #12, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key 1, keypos = 1, keylen = 63,              ~
                            key 2, keypos = 17, keylen = 47

            select #13, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #14, "WCMASTR",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =  2  , keylen = 5 ,                      ~
                        alt key  1, keypos = 1, keylen = 6

            select #15, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 35

            select #16, "TXTFILE" ,                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen = 11

            select #17, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, " ")
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, " ")
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, " ")
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, " ")
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, " ")
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, " ")
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, " ")
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, " ")
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, " ")
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, " ")
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, " ")
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, " ")
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, " ")
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, " ")
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, " ")
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, " ")
            call "OPENCHCK" (#17, fs%(17), f2%(17), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


           REM If DRIVER% = 0% then DEMSTAT will goto input after the    ~
               display screen.
             driver% = 0%

         call "DEMSTAT"( demstring$, #1, #2, #3, #4, #5, #6, #7, #8,     ~
                                     #9, #10, #11, #12, #13, #14,        ~
                                     #15, #16, #17, return%, driver%)

             return% = return%

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "SHOSTAT" ("One Moment Please")

            end
