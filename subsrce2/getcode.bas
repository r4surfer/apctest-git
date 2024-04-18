        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   EEEEE  TTTTT   CCC    OOO   DDDD   EEEEE          *~
            *  G      E        T    C   C  O   O  D   D  E              *~
            *  G GGG  EEEE     T    C      O   O  D   D  EEEE           *~
            *  G   G  E        T    C   C  O   O  D   D  E              *~
            *   GGG   EEEEE    T     CCC    OOO   DDDD   EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GETCODE  -  Works exactly like describe, except that if   *~
            *             the code passed is not on file, the user is   *~
            *             put into an inquiry mode that allows him to   *~
            *             search for the code he wants.                 *~
            *             Searchs on descriptions are allowed if these  *~
            *             conditions are met...                         *~
            *          A) Description must immediately follow prime key *~
            *          B) Description must be an alternate key in file  *~
            *          C) Alt key must be in the file select of caller  *~
            *          D) Description length *should* be less then 51   *~
            *          E) Description must be described correctly in    *~
            *             'CALL' statement of caller (see below).       *~
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
            * 01/18/84 ! ORIGINAL (SEMI-CONED FROM DESCRIBE)      ! HES *~
            * 03/20/85 ! Re-formatted screen, no more X box logic ! HES *~
            * 04/22/85 ! Added WSXIO                              ! HES *~
            * 05/09/85 ! Added Scroll Feature                     ! HES *~
            * 07/15/85 ! Added special provisions for GLMAIN file ! HES *~
            * 09/26/86 ! Added LDJ's Soundex Routine              ! HES *~
            * 10/04/86 ! Modified Call to SCREEN - now compatible ! LDJ *~
            *          !   with O.S. 7.10.                        !     *~
            * 03/16/87 ! Replaced call to RJuSTIFY w/ STRING      ! ERN *~
            * 05/22/87 ! Support Of Obsolete Flag On GLAMIN       ! HES *~
            * 09/10/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 04/20/89 ! If HNYMASTR, enable ALT KEY on descr.    ! JDH *~
            *          !    Done to Domestic 09/15/87 by HES      !     *~
            * 10/23/89 ! OS 7.20/30 Compatability (KAB 4-19-89)   ! MJB *~
            * 02/20/91 ! Converted to stub for ZGETCODE           ! MJB *~
            *************************************************************

            sub "GETCODE" (arg1, arg2, arg3, arg4, arg5, arg6)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZGETCODE" (arg1, arg2, arg3, arg4, arg5, arg6)

            end
