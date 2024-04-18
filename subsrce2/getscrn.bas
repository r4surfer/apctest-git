        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   GGGG  EEEEE  TTTTT   SSS    CCC   RRRR   N   N          *~
            *  G      E        T    S      C   C  R   R  NN  N          *~
            *  G GGG  EEE      T     SSS   C      RRRR   N N N          *~
            *  G   G  E        T        S  C   C  R   R  N  NN          *~
            *   GGG   EEEEE    T     SSS    CCC   R   R  N   N          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GETSCRN  - Replacement for SCREEN USERSUB.  Avoids having *~
            *            to pass full screen image when only cursor     *~
            *            position is required. Important for remotes.   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/88 ! ORIGINAL                                 ! KAB *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 02/20/91 ! Converted to stub for ZGETSCRN           ! MJB *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            sub "GETSCRN" (arg1, arg2, arg3, arg4)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZGETSCRN" (arg1, arg2, arg3, arg4)

            end
