        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   OOO   PPPP   EEEEE  N   N   OOO   L      IIIII  BBBB    *~
            *  O   O  P   P  E      NN  N  O   O  L        I    B   B   *~
            *  O   O  PPPP   EEEE   N N N  O   O  L        I    BBBB    *~
            *  O   O  P      E      N  NN  O   O  L        I    B   B   *~
            *   OOO   P      EEEEE  N   N   OOO   LLLLL  IIIII  BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * OPENOLIB - Opens files using the OUTLIB rather than the   *~
            *            INLIB.  Does so by setting the INLIB equal to  *~
            *            the current OUTLIB value, calling OPENFILE, and*~
            *            then resetting the INLIB back to it's original *~
            *            value, i.e. this routine is nothing more than a*~
            *            driver to OPENFILE.  (Calling program should   *~
            *            use same calling syntax as OPENFILE).          *~
            *            Note; this routine used primarily by the SES & *~
            *            SAS utilities.                                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/26/84 ! ORIGINAL (COPIED OPENFILE)               ! HES *~
            * 03/05/85 ! Stripped to be nothing more than a Driver! LDJ *~
            *          !   for OPENFILE.                          !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "OPENOLIB" (#1, mode$, f2%, returncode$, axd$)

        dim                                                              ~
            curlib$8,                    /* CURRENT INPUT LIBRARY      */~
            mode$5,                      /* INPUT, OUTPUT, SHARE, ETC. */~
            returncode$20,               /* ERROR MESSAGE IF ANY       */~
            outlib$8,                    /* DEFAULT OUTPUT LIBRARY NAME*/~
            axd$4                        /* POINTER TO FIRST ALT KEY.  */~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.14.16 08/27/85 Menu modifications              "
        REM *************************************************************

            if mode$ = "OUTPUT" and returncode$ = " " then returncode$ = ~
               "OUTPTP00002000080080"

        REM *************************************************************~
            *            Get current INLIN and OUTLIB values            *~
            * Set the INLIB = OUTLIB, Call OPENFILE, Set the INLIB back *~
            *************************************************************
            if outlib$ > " " and f2% >= 0% then L09600
            if outlib$ = " " and f2% > 0% then f2% = -f2%
            call "EXTRACT" addr("IL",curlib$,"OL",outlib$)
L09600:     call "SET" addr("IL",outlib$)
            call "OPENFILE" (#1, mode$, f2%, returncode$, axd$)
            call "SET" addr("IL",curlib$)
            end
