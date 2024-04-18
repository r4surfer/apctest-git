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
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            sub "ZGETSCRN" (function$,   /* Function -                 */~
                                         /*   " ", C =  Cursor Only    */~
                                         /*        I =  Screen Image   */~
                                         /*        B =  Both           */~
                           i$(),         /* Image Reciever, May be     */~
                                         /*    literal " " if image    */~
                                         /*    not requested.          */~
                           cursor%(),    /* Cursor Reciever, 2*BI(4)   */~
                           return%)      /* WSXIO Open Return Code     */~

        dim iocw$(1)4,                   /* IO Control Word            */~
            j$(24)80,                    /* Screen Image               */~
            iosw$8,                      /* IO Status Word             */~
            cursor%(2),                  /* Cursor Row, Column         */~
            function$1,                  /* Function                   */~
            i$(24)80                     /* Screen Image               */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01072
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
L01072: REM *************************************************************
            select #1, "CRTFILE",        /* CRT UFB for WSXIO          */~
                        consec, recsize = 1924

            return% = 99%
            rlen% = 0%
            if function$ = "I" or function$ = "B" then rlen% = 1920%
            mat cursor% = zer

            close ws
            call "WSXIO" addr("O", 255%, "Y", #1, return%)
*             IF RETURN% <> 0% THEN END
            iocw$(1) = hex(01000000)
            call "WSXIO" addr("X", #1, hex(40), iocw$(1), j$(), rlen%,   ~
                               iosw$)
            call "WSXIO" addr("C", #1)

            if function$ = "I" or function$ = "B" then                   ~
               str(i$()) = str(j$())
            if function$ = "I" then end

            cursor%(1) = val(str(iocw$(1),4%,1%),1)
            cursor%(2) = val(str(iocw$(1),3%,1%),1)

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
