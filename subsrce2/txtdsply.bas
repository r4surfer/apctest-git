        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  DDDD    SSSS  PPPP   L      Y   Y   *~
            *    T     X X     T    D   D  S      P   P  L       Y Y    *~
            *    T      X      T    D   D   SSS   PPPP   L        Y     *~
            *    T     X X     T    D   D      S  P      L        Y     *~
            *    T    X   X    T    DDDD   SSSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTDSPLY - Subroutine to allow displaying of text. See    *~
            *            below for more notes.                          *~
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
            * 08/28/85 ! ORIGINAL                                 ! ERN *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            *          !   Also Minor bug in print logic.         !     *~
            * 02/20/91 ! Converted to stub for ZTXTDSPL           ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "TXTDSPLY" (arg1, arg2, arg3, arg4, arg5, arg6)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZTXTDSPL" (arg1, arg2, arg3, arg4, arg5, arg6)

            end
