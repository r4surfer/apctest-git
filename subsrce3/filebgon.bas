        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF  IIIII  L      EEEEE  BBBB    GGG    OOO   N   N   *~
            *  F        I    L      E      B   B  G      O   O  NN  N   *~
            *  FFF      I    L      EEE    BBBB   G      O   O  N N N   *~
            *  F        I    L      E      B   B  G  GG  O   O  N  NN   *~
            *  F      IIIII  LLLLL  EEEEE  BBBB    GGG    OOO   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FILEBGON = FILE - B - GONe, the all in one programmers    *~
            *            tool to zap unwanted data files, sending them  *~
            *            off to that great bit bucket in the sky.       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/29/86 ! Original                                 ! HES *~
            * 03/15/91 ! Change CLOSE/GETNAMES order so files     ! KAB *~
            *          ! Organized as 'PRINT' work as advertized  !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


            sub "FILEBGON" (#1)

        dim                                                              ~
            file$8,                      /* da file                    */~
            lib$8,                       /* da library                 */~
            vol$6                        /* da volume                  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 03/15/91 BASIC 4.03.01 & SSL Support     "

        REM *************************************************************~
            *               S C R A T C H    F I L E                    *~
            *-----------------------------------------------------------*~
            * So long sucker...                                         *~
            *************************************************************

            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "GETUFBS1" addr(#1, f2%)
            if f2% = 1% then close #1
            call "SCRATCH" addr("F", file$, lib$, vol$, "B", " ", 0%)
            rem  THE:end  /* It was fun while it lasted */
