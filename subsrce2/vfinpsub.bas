        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  FFFFF  IIIII  N   N  PPPP    SSS   U   U  BBBB    *~
            *  V   V  F        I    NN  N  P   P  S      U   U  B   B   *~
            *  V   V  FFFF     I    N N N  PPPP    SSS   U   U  BBBB    *~
            *   V V   F        I    N  NN  P          S  U   U  B   B   *~
            *    V    F      IIIII  N   N  P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VFINPSUB - Handles Variable Fields screens for files      *~
            *            defined in SYSFILE2 'VF'.                      *~
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
            * 05/23/86 ! ORIGINAL                                 ! ERN *~
            * 08/26/87 ! Ability to edit against GENCODES added.  ! ERN *~
            * 02/09/88 ! REMOVE DUPLICATE CMS2V VARIABLE          ! BPN *~
            * 02/20/91 ! Converted to stub for ZVFINPSU           ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "VFINPSUB" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZVFINPSU" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

            end
