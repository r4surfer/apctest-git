        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  EEEEE  N   N   AAA   BBBB   L       SSS   U   U  BBBB    *~
            *  E      NN  N  A   A  B   B  L      S      U   U  B   B   *~
            *  EEEE   N N N  AAAAA  BBBB   L       SSS   U   U  BBBB    *~
            *  E      N  NN  A   A  B   B  L          S  U   U  B   B   *~
            *  EEEEE  N   N  A   A  BBBB   LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ENABLSUB - This routine performs most of the tasks        *~
            *            related to setting and resetting 'soft coded'  *~
            *            Default/Enable switches.                       *~
            *            NOTE- Calling program should ensure that the   *~
            *             user attempting to modify settings is either  *~
            *             a module or data base administrator.          *~
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
            * 09/26/85 ! ORIGINAL                                 ! ERN *~
            * 02/20/91 ! Converted to stub for ZENABLSU           ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ENABLSUB" (arg1, arg2, arg3, arg4, arg5, arg6,              ~
                        arg7, arg8)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

        close ws

        call "ZENABLSU" (arg1, arg2, arg3, arg4, arg5, arg6,             ~
                         arg7, arg8)

        end
