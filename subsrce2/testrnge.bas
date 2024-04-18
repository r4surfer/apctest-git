        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  EEEEE   SSS   TTTTT  RRRR   N   N   GGG   EEEEE   *~
            *    T    E      S        T    R   R  NN  N  G      E       *~
            *    T    EEEE    SSS     T    RRRR   N N N  G GGG  EEEE    *~
            *    T    E          S    T    R   R  N  NN  G   G  E       *~
            *    T    EEEEE   SSS     T    R   R  N   N   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TESTRNGE - Now a stub for ZTESTRNG.  Calling format       *~
            *            depends on whether 5,6,or 7 arguments passed in*~
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
            * 06/16/86 ! Original                                 ! ERN *~
            * 07/09/92 ! If '?'(Wildcard) is the last character   ! JDH *~
            *          !   and channel was passed in, uses GETCODE!     *~
            * 11/16/92 ! Changed to stub.                         ! JDH *~
            * 04/11/94 ! Added ability to pass 7 args to ZTESTRNG.! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "TESTRNGE"  (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

        REM *************************************************************~
            *             P R E F O R M   T E S T                       *~
            *-----------------------------------------------------------*~
            * Test the number of arguments passed in.                   *~
            *************************************************************

            call "NARGS" addr(args%) /* # of arguments passed in.  If */
                                     /* 6 then channel passed         */
                                     /* for GETCODE selection screen. */
                                     /* 7 then arg for GENCODES.      */

            close ws

            if args% = 5% then                                           ~
                call "ZTESTRNG" (arg1, arg2, arg3, arg4, arg5)
            if args% = 6% then                                           ~
                call "ZTESTRNG" (arg1, arg2, arg3, arg4, arg5, arg6)
            if args% = 7% then                                           ~
                call "ZTESTRNG" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)
            end

