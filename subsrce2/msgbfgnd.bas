        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  M   M   SSS    GGG   BBBB   FFFFF   GGG   N   N  DDDD    *~
            *  MM MM  S      G      B   B  F      G      NN  N  D   D   *~
            *  M M M   SSS   G GGG  BBBB   FFFF   G GGG  N N N  D   D   *~
            *  M   M      S  G   G  B   B  F      G   G  N  NN  D   D   *~
            *  M   M   SSS    GGG   BBBB   F       GGG   N   N  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MSGBFGND - Control Message Port for Background Tasks      *~
            *            which might be running in forground.  Remove   *~
            *            Need for HELP-16 Exit from foreground by       *~
            *            unlocking keyboard and setting up exit with    *~
            *            either PF16 or PF32                            *~
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
            * 09/16/92 ! Original                                 ! KEN *~
            * 09/30/94 ! PRR-13295 Don't iterate the Message Port ! RJH *~
            *          !   Count if the Message is recieved.      !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "MSGBFGND" (show$, port$, message$, mlen%, idle%, ret%)

*        Subs for:
*        CALL "SHOSTAT" ("Task ????? Standing By") /* SHOW$ */
*        CALL "MESSAGE" ADDR("CH",PORT$,"T",6000%,MESSAGE$,MLEN%,RET%)
*        INACTIVE% = INACTIVE% + 1%                /* IDLE% */

        dim                              /* WSXIO/Screen IO Variables  */~
            order$(1)4,                  /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw$8,                      /* IO Word Status Bytes       */~
            tt$,                         /* Task Type, B or F          */~
            inshow$80,                   /* SHOSTAT Message, Internal  */~
            pgmid$8,                     /* Program Id                 */~
                                                                         ~
            show$80,                     /* SHOSTAT Message            */~
            port$4,                      /* Port Id                    */~
            message$10                   /* Message Receiver           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #64 ! CRTFILE  ! Work Station UFB                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #64, "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924

*        Very Little Set-up Needed

            if tt$ = " " then call "EXTRACT" addr("TT", tt$, "CF", pgmid$)

*        All Background Tasks have a "SHOSTAT" to key from.
*        REM Go into stand by mode           60 seconds

            if tt$ = "F" then L11000

*        Regular Message Call
            call "MESSAGE" addr("CH",port$,"T",6000%,message$,mlen%,ret%)
            goto L65000

L11000
*        Running in Foreground, let's get fancy

            inshow$ = show$
            if inshow$ <> " " then L11050
               inshow$ = "Task " & pgmid$ & " Standing By"
L11050:     if len(inshow$) >= 40% then L11070
               str(inshow$,41%) = "Press PF16 to Terminate"
L11070:     call "SHOSTAT" (inshow$)

            call "WSXIO" addr("O", 255%, "Y", #64, ret%)
            order$(1%) = hex(01a00101)     /* Unlock the Keyboard */
            call "WSXIO" addr("X", #64, hex(80), order$(), s$(),     0%, ~
                              iosw$)

            call "MESSAGE" addr("CH",port$,"B",5900%,message$,mlen%,ret%)

            order$(1%) = hex(01000000)     /* Lock the Keyboard   */
               if ret% <> 12% then L11270

            call "WSXIO" addr("X", #64, hex(40), order$(), s$(),    0%,  ~
                              iosw$)

            if str(iosw$,3%,1%) = "P" then ret%   = 16%  /* U/Cs P = 16 */
            if str(iosw$,3%,1%) = "p" then ret%   = 16%  /* L/Cs p = 32 */
            if str(iosw$,3%,1%) = "a" then idle% =  0%   /* L/Cs a = 17 */
               goto L11300

L11270:     call "WSXIO" addr("X", #64, hex(80), order$(), s$(),    0%,  ~
                              iosw$)

L11300:     call "WSXIO" addr("C", #64)

L65000: REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG

*          IDLE% = IDLE% + 1%
            if ret% <> 0% then idle% = idle% + 1% /* Don't iterate if  */
            end                                   /*  Message Received */
