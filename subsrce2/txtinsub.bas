        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  IIIII  N   N   SSSS  U   U  BBBB    *~
            *    T     X X     T      I    NN  N  S      U   U  B   B   *~
            *    T      X      T      I    N N N   SSS   U   U  BBBB    *~
            *    T     X X     T      I    N  NN      S  U   U  B   B   *~
            *    T    X   X    T    IIIII  N   N  SSSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTINSUB - Subroutine to allow entry and modification of  *~
            *            text.  The routine is intended to be general   *~
            *            enough to satisfy most text requirements that  *~
            *            exist in CMS. See below for more notes.        *~
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
            * 02/20/91 ! Converted to stub for ZTXTINSU           ! MJB *~
            * 07/08/91 ! PRR 12085.  Added CLOSE WS.              ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "TXTINSUB" (arg1, arg2, arg3, arg4, arg5, arg6)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            close ws
            call "ZTXTINSU" (arg1, arg2, arg3, arg4, arg5, arg6)

            end
