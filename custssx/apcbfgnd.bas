        REM *************************************************************~
            *                                                           *~
            *  Subroutine - APCBFGND          Caelus Version = MSGBFGND *~
            *                                                           *~
            *  DATE       - 05/26/97                                    *~
            *                                                           *~
            *  BY         - ROY H. HOFFMAN                              *~
            *                                                           *~
            *  SLEEP%     - New Argument to set the Sleep time interval *~
            *               to 60 Seconds = 6000%.                      *~
            *                                                           *~
            *  Called By  - JBPOST1 and JBPOST2                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCBFGND - Control Message Port for Background Tasks      *~
            *            which might be running in forground.  Remove   *~
            *            Need for HELP-16 Exit from foreground by       *~
            *            unlocking keyboard and setting up exit with    *~
            *            either PF16 or PF32                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/26/97 ! AS OF                                    ! RHH *~
            *************************************************************

            sub "APCBFGND" (show$, port$, message$, mlen%, idle%,        ~
                                                           sleep%, ret% )
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
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
*        REM Go into stand by mode           60 Seconds
*        REM EWD SLEEP% = 300%                3 Seconds    /* JBPOST1 */
*        REM                                 60 Seconds    /* JBPOST2 */
            if tt$ = "F" then L00820

*        Regular Message Call
*          CALL "MESSAGE" ADDR("CH",PORT$,"T",6000%,MESSAGE$,MLEN%,RET%)
            call "MESSAGE" addr("CH",port$,"T",sleep%,message$,mlen%,ret%)
            goto L01140

L00820
*        Running in Foreground, let's get fancy

            inshow$ = show$
            if inshow$ <> " " then L00870
               inshow$ = "Task " & pgmid$ & " Standing By"
L00870:     if len(inshow$) >= 40% then L00890
               str(inshow$,41%) = "Press PF16 to Terminate"
L00890:     call "SHOSTAT" (inshow$)

            call "WSXIO" addr("O", 255%, "Y", #64, ret%)
            order$(1%) = hex(01a00101)     /* Unlock the Keyboard */
            call "WSXIO" addr("X", #64, hex(80), order$(), s$(),     0%, ~
                              iosw$)

            call "MESSAGE" addr("CH",port$,"B",5900%,message$,mlen%,ret%)

            order$(1%) = hex(01000000)     /* Lock the Keyboard   */
               if ret% <> 12% then L01090

            call "WSXIO" addr("X", #64, hex(40), order$(), s$(),    0%,  ~
                              iosw$)

            if str(iosw$,3%,1%) = "P" then ret%   = 16%  /* U/Cs P = 16 */
            if str(iosw$,3%,1%) = "p" then ret%   = 16%  /* L/Cs p = 32 */
            if str(iosw$,3%,1%) = "a" then idle% =  0%   /* L/Cs a = 17 */
               goto L01120

L01090:     call "WSXIO" addr("X", #64, hex(80), order$(), s$(),    0%,  ~
                              iosw$)

L01120:     call "WSXIO" addr("C", #64)

L01140: REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
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

            idle% = idle% + 1%
            end
