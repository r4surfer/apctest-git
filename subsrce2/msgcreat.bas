        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  M   M   SSS    GGG    CCC   RRRR   EEEEE   AAA   TTTTT   *~
            *  MM MM  S      G      C   C  R   R  E      A   A    T     *~
            *  M M M   SSS   G GGG  C      RRRR   EEEE   AAAAA    T     *~
            *  M   M      S  G   G  C   C  R   R  E      A   A    T     *~
            *  M   M   SSS    GGG    CCC   R   R  EEEEE  A   A    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MSGCREAT - Handle Digging up Port Id & Creating Message   *~
            *            Port.  If unsuccessful, throws GETPARM either  *~
            *            to USER (foreground) or Operator (background)  *~
            *            prior to Exiting with appropriate Return Code  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/18/92 ! Original                                 ! KEN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "MSGCREAT" (#1, port$, syskey$, return%)

        dim                                                              ~
            port$4,       /* Port Id (Returned)                        */~
            syskey$20,    /* Sysfile 2 Key (Optional, blank OK)        */~
                                                                         ~
            gp$2,         /* Getparm Type (Operator or User)           */~
            msg1$80,      /* Getparm Message                           */~
            msg2$80,      /* Getparm Message                           */~
            msg3$80,      /* Getparm Message                           */~
            progid$8,     /* Program ID                                */~
            readkey$20,   /* G/P Readkey                               */~
            sys$6,        /* System Version                            */~
            tt$           /* Task Type B or F (Wang), X (Unix)         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! System File                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("TT", tt$, "S#", sys$, "CF", progid$)

            convert str(sys$,,6%) to f1%, data goto L09100
               goto L09120                 /* Running on WANG */
L09100:     tt$ = "X"                    /* Running on Unix */

L09120:     readkey$ = syskey$
            if readkey$ <> " " then L10000
               readkey$ = "PORT.ID." & progid$

L10000: REM *************************************************************~
            *                 M A I N   P R O G R A M                   *~
            *-----------------------------------------------------------*~
            * Handles normal data Processing.                           *~
            *************************************************************

*        Dig Up Port Id For Inter Task Messaging...
            call "READ100" (#1, readkey$, f1%)
               if f1% = 0% then port_error
            get #1, using L10160, port$
L10160:         FMT XX(20), CH(4)

*        Set up message port so that people can tell we're running
            call "MESSAGE" addr ("CR", port$, 200%, return%)
                if return% = 0% then exit_program
            goto port_create_error

        REM *************************************************************~
            * ASK USER ERROR ACKNOWLEDGEMNTS AND BAIL-OUTS              *~
            *************************************************************

        port_error:

            msg1$ = "Unable to Find Port I.D. in System Control File."
            msg2$ = " "
            return% = 99%
            goto call_getparm

        port_create_error:

            msg1$ = "Unable to Create System Message Port!"
            msg2$ = "This program may be running in BACKGROUND or "
            msg2$ = msg2$ & "on another terminal."

        call_getparm:
            if tt$ = "B" then gp$ = "O " else gp$ = "I "
            msg3$ = "Press ENTER to Exit Program."
            call "GETPARM" addr(gp$, "A", progid$, " ", "0001",          ~
                                str(progid$,,6%), 3%,                    ~
                                msg1$, 70%, msg2$, 70%, msg3$, 70%)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
