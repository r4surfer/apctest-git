        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   M   M   SSS   M   M   AAA   IIIII  N   N  PPPP    *~
            *  C   C  MM MM  S      MM MM  A   A    I    NN  N  P   P   *~
            *  C      M M M   SSS   M M M  AAAAA    I    N N N  PPPP    *~
            *  C   C  M   M      S  M   M  A   A    I    N  NN  P       *~
            *   CCC   M   M   SSS   M   M  A   A  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSMAINP - Allows A/C/D of Module Administrator List for  *~
            *            Module specified.                              *~
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
            * 12/26/85 ! ORIGINAL                                 ! ERN *~
            * 02/20/91 ! Converted to stub for ZCMSMAIN           ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CMSMAINP" (arg1, arg2)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZCMSMAIN" (arg1, arg2)

            end
