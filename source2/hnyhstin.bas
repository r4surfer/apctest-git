        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  H   H   SSS   TTTTT  IIIII  N   N   *~
            *  H   H  NN  N  Y   Y  H   H  S        T      I    NN  N   *~
            *  HHHHH  N N N   YYY   HHHHH   SSS     T      I    N N N   *~
            *  H   H  N  NN    Y    H   H      S    T      I    N  NN   *~
            *  H   H  N   N    Y    H   H   SSS     T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYHSTIN - Setup or edit the HNYHSTRY records created by  *~
            *            HNYPST2                                        *~
            *----------------------------------------------------------Q*~
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
            * 02/09/87 ! Original                                 ! HES *~
            * 05/13/87 ! Std Cost Changes                         ! ERN *~
            * 09/01/87 ! Now a driver to call HNYHSTG or HNYHSTF  ! LKM *~
            *          ! depending on flag set in HNYFLAGS        !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cal$1                        /* Calendar Type              */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! STORNAME ! STORE INFORMATION FILE                   *~
            * # 3 ! HNYHSTRY ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 2, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select # 3, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 3, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            call "READ100" (#3, "SWITCHS.HNY", f1%(3))
                  if f1%(3) <> 1 then L10120
                     get #3 using L10100, cal$
L10100:              FMT POS(95), CH(1)
                  if cal$ <> " " then L10180
L10120:           kh% = 2
                  call "ASKUSER" (kh%,"HISTORY RETENTION FLAG NOT VALID",~
                  "The History Retention Flag Must be set to 'F', 'G' or ~
        ~'B' in HNYFLAGS.", "Press any PF key to acknowledge.", " ")
                 goto L65000

L10180:     if cal$ = "G" then L10261
            if cal$ = "F" then L10264

L10210:         kh% = 2
                call "ASKUSER" (kh%, "CHOOSE CALENDAR STRUCTURE","Press P~
        ~F1 for Gregorian Calendar", "- or -", "Press PF2 for Fiscal Calen~
        ~dar.")
                if kh% = 1 then L10261
                if kh% = 2 then L10264
                goto L10210

L10261:     call "HNYHSTG" (#1, #2)
            goto L65000

L10264:     call "HNYHSTF" (#1, #2, #3)
            goto L65000

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

            call "SHOSTAT" ("One Moment Please")

            end
