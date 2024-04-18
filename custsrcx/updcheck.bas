        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  U   U  PPPP   DDDD    CCC   H   H  EEEEE   CCC   K   K   *~
            *  U   U  P   P  D   D  C   C  H   H  E      C   C  K  K    *~
            *  U   U  PPPP   D   D  C      HHHHH  EEEE   C      KKK     *~
            *  U   U  P      D   D  C   C  H   H  E      C   C  K  K    *~
            *   UUU   P      DDDD    CCC   H   H  EEEEE   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDCHECK - 1) CHECKS IF USER HAS AN PROCEDURE IN PROCESS; *~
            *                 YES- (--> RESTARTING) INFORMS OF RESTART &*~
            *                      EXECUTES (OR ABORTS).                *~
            *            2) LOGS CURRENT PROCEDURE INTO 'UPDCTL'.       *~
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
            * 04/04/84 ! ORIGINAL                                 ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim currproc$8,                  /* CONTROLLING PROCEDURE      */~
            restartproc$8,               /* PROCEDURE TO RESTART       */~
            restartprog$8,               /* PROGRAM TO RESTART         */~
            restartsub$8,                /* SUBRTN TO RESTART          */~
            savenrs(5),                  /* RESTART SAVE NUMBERS       */~
            savestrs$(5)50,              /* RESTART SAVE STRINGS       */~
            transkey$100,                /* RESTART TRANSACTION KEY    */~
            userid$3                     /* USER ID                    */

        dim f2%(64)                      /* FILE STATUS FLAGS          */

            mat f2% = con : cms2v$  = 'R41209   ' : cms2v$ = ' '

        REM *************************************************************~
            *   S E L E C T   S T A T E M E N T S                       *~
            *************************************************************

            select #1, "UPDCTL",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1,  keylen = 3


        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *************************************************************
        mainlogic
          /* First see if anything for this user was in-process in     */
          /* a prior life-time. Note that sub-routine opens the file   */
          /* 'UPDCTL' and informs user if restart is required.         */

            call "UPDSUB" ("C", #1, f2%(1), restartproc$, restartprog$,  ~
                           restartsub$, step%, substep%, transkey$,      ~
                           savenrs(), savestrs$(), userid$, return%)

          /*  Get this procedure's ID    */
            if linkcnt% > 0 then goto L10190
/*
            call "GETPARM" addr ("I ", "R", "PROCID  ", " ", "0000",     ~
                                 "UPDCHK", "PROCEDURE NAME: ", 16%,      ~
                                 "K", "PROCID  ", currproc$, 8%,         ~
                                 0%, 0%, "A")
*/
          currproc$ = "PROCARIU"

L10190:   /* If RETURN% = 0% then no restart is required. Just log     */
          /* this guy on and continue along our merry way.....         */
        logproc
            if return% <> 0% then goto restart
            init(" ") restartprog$, restartsub$, savestrs$(), transkey$
            mat savenrs = zer
            step%, substep% = 0%

            call "UPDSUB" ("W", #1, f2%(1), currproc$, "UPDCHECK", " ",  ~
                           0%, 0%, " ", savenrs(), savestrs$(), userid$, ~
                           return%)

            return% = 0%
            goto L65000


        restart
          /* A restart is called for. If it is not for this procedure, */
          /* execute proper procedure before continuing. If it is for  */
          /* this procedure, inform user and get cont/abort response.  */

            if currproc$ = restartproc$ then goto restartthis
            if linkcnt% <> 1% then goto L10420
              /* Already linked to this guy once and his in-process    */
              /* status record has not been removed. Let us assume     */
              /* that this means user requested that the restart be    */
              /* aborted and just get out of here (endless loops are   */
              /* frustrating). If the restarted procedure bombed and   */
              /* was cancelled, well, something else than restarting   */
              /* from the wrong spot (ie, here) needs to be done in    */
              /* any event.                                            */
                return% = 2%
                goto L65000

L10420:     close ws
            close #1 :  f2%(1) = 1%
            call "LINK" addr (restartproc$, " ", "        ",             ~
                                  "      ", 0%, "C", "CANCEL PROCESSING",~
                                  17%)
            linkcnt% = 1%  /* Flag that we linked to another proc      */
            goto mainlogic /* Start all over again.........            */

          restartthis
            call "UPDSUB" ("I", #1, f2%(1), restartproc$, restartprog$,  ~
                           restartsub$, step%, substep%, transkey$,      ~
                           savenrs(), savestrs$(), userid$, return%)

            if return% = 0% then goto logproc      /* ???              */
            goto L65000         /* Normal. RETURN% --> continue/abort   */

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%

            end  return%
