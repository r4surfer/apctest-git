        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  L       OOO   TTTTT  V   V   AAA   L      IIIII  DDDD    *~
            *  L      O   O    T    V   V  A   A  L        I    D   D   *~
            *  L      O   O    T    V   V  AAAAA  L        I    D   D   *~
            *  L      O   O    T     V V   A   A  L        I    D   D   *~
            *  LLLLL   OOO     T      V    A   A  LLLLL  IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTVALID - Validate new Lot Number passed in to ensure    *~
            *            that; A) it is a unique lot number, and        *~
            *            B) that the format is valid as per the format  *~
            *            defined by HNYFLAGS, and C) that lot numbers   *~
            *            (tracking) is allowed for the given part.      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/87 ! Original                                 ! LDJ *~
            * 02/20/91 ! Converted to stub for ZLOTVALI           ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "LOTVALID" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZLOTVALI" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)


            end
