        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  U   U  PPPP   DDDD   N   N  EEEEE  X   X  TTTTT          *~
            *  U   U  P   P  D   D  NN  N  E       X X     T            *~
            *  U   U  PPPP   D   D  N N N  EEEE     X      T            *~
            *  U   U  P      D   D  N  NN  E       X X     T            *~
            *   UUU   P      DDDD   N   N  EEEEE  X   X    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDNEXT  - RESTART CONTROL MODULE ROUTINE.  CHECKS IF NEXT*~
            *            PROGRAM IN PROCEDURE IS TO RUN OR NOT. THIS IS *~
            *            DONE BY RETRIEVING PREVIOUS-PGM AND NEXT-PGM   *~
            *            FROM PROC AND COMPARING THIER VALUES TO THE    *~
            *            PROGRAM FROM THE IN-PROCESS CONTROL RECORD.    *~
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
            * 04/09/84 ! ORIGINAL                                 ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim procedureid$8,               /* RESTART: Controlling Proc  */~
            programid$8,                 /*          Controlling Prog  */~
            subrtnid$8,                  /*          restart sub-rtn   */~
            transkey$100,                /*          transaction key   */~
            savenrs(5),                  /*          Save numbers      */~
            savestrs$(5)50,              /*          Save strings      */~
            prevpgm$8,                   /*          S/B previous pgm  */~
            nextpgm$8,                   /*          potenial next pgm */~
            userid$3                     /* User-ID                    */~

        dim f2%(64)                      /* FILE STATUS FLAGS FOR      */

            mat f2% = con : cms2v$ = 'R41209   ' : cms2v$ = ' '

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * #60 ! UPDCTL   ! RESTART CONTROL FILE                     *~
            *************************************************************

            select #60,"UPDCTL",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1,  keylen = 3

        REM *************************************************************~
            *        I N I T I A L I Z A T I O N                        *~
            *************************************************************

            status% = 0%  /* Set for good return (next pgm to run)     */

          /* Do GETPARM to retrieve PREVPGM & NEXTPGM from proc        */
            call "GETPARM" addr ("I ", "R", "GETPGMS ", " ", "0001",     ~
                                 "UPDNXT", "PROGRAM SEQUENCE", 16%,      ~
                                 "K", "PREVPGM ", prevpgm$,8%,0%,0%,"A", ~
                                 "K", "NEXTPGM ", nextpgm$,8%,0%,0%,"A")

        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *~
            *      R E S T A R T   C H E C K   L O G I C                *~
            *                                                           *~
            *  Check to see if 'nextpgm' is really the one that is to   *~
            *  be run. Logic is as follows--                            *~
            *                                                           *~
            *    - If no in-process record (RETURN% = 0%) then restart  *~
            *      sub-system is disabled. (--> run next)               *~
            *    - If in-process record shows next program as the       *~
            *      controlling (current) program, then this program     *~
            *      is the one that needs to restart. (--> run next)     *~
            *  -ELSE-                                                   *~
            *    - If current program from in-process record = the      *~
            *      previous program (from the proc) then next program   *~
            *      is starting for the first time. (Log on & run).      *~
            *  -ELSE-                                                   *~
            *    - Next program has already ran and completed. The      *~
            *      restart point must be further down the line.         *~
            *      (--> don't run next)                                 *~
            *************************************************************


         /* Get information from in-process record                     */
            call "UPDSUB" ("C", #60, f2%(60), procedureid$, programid$,  ~
                           subrtnid$, step%, substep%, transkey$,        ~
                           savenrs(), savestrs$(), userid$, return%)

            if return% = 0% then L65000   /* Restart sub-system disabled*/

         /* See if next program was already running.                   */
            if programid$ = nextpgm$ then goto L65000 /* Continue w/next*/

         /* If what should have been the previous-program is, then     */
         /* we are next will be running for the first time.            */

            if programid$ <> prevpgm$ then goto dont_start_next

         /* Log next program in as the current program. (1st time thru)*/
                programid$ = nextpgm$
                init (" ") subrtnid$, transkey$, savestrs$()
                step%, substep% = 0%
                mat savenrs = zer

                call "UPDSUB" ("W",#60,f2%(60), procedureid$, programid$,~
                               subrtnid$, step%, substep%, transkey$,    ~
                               savenrs(), savestrs$(), userid$, return%)
                goto L65000

        dont_start_next
            /* Do not need to re-run the next program. Just exit so    */
            /* that procedure will continue with next program(s). One  */
            /* of them had better be the right guy.                    */
                status% = 1%   /* Flag that nextpgm is not to run      */
                goto L65000

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
                if f2%(u3%) = 0% then close # u3%
                next u3%

            end   status%
