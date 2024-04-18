        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGGG  U   U  IIIII  V   V  BBBB   RRRR   U   U  N   N   *~
            *  G      U   U    I    V   V  B   B  R   R  U   U  NN  N   *~
            *  G GGG  U   U    I    V   V  BBBB   RRRR   U   U  N N N   *~
            *  G   G  U   U    I     V V   B   B  R   R  U   U  N  NN   *~
            *   GGG    UUU   IIIII    V    BBBB   R   R   UUU   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GUIVBRUN - This routine will send the CoStar API command  *~
            *            to run a VB application in the caelus\vbapps   *~
            *            directory or on the user's path.               *~
            *            If the flag is set the routine will            *~
            *            wait a specified time for a response before    *~
            *            returning to the caller.                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1995, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/06/95 ! Original                                 ! LDJ *~
            * 04/30/97 ! Replace WSXIO with GETCMD for NT.        ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GUIVBRUN" (function$,    /* name of VB application to run */~
                        timeout%,     /*  0 = use default wait,        */~
                                      /* <0 = don't wait at all        */~
                                      /* >0 = wait for specified # secs*/~
                        return%)      /* >= 0 = Application responded  */


        dim                                                              ~
            cdir$32,                     /* PC Files directory         */~
            function$16,                 /* Program/subroutine name    */~
            temp$2,                      /* GETCMD arg                 */~
            uw$1                         /* CoStar MAGIC Character     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *** Are we in GUI Land ? ***
            call "CHECKGUI" addr(gui%)
            if gui% = 0% then exit_routine
            uw$ = hex(7f)
            cdir$="." & hex(5c) & "caelus" & hex(5c) & "vbapps" & hex(5c)
            call "SENDCMD" (uw$ & "UWMSGNow loading " & cdir$ & function$~
                            & uw$)
            REM *** Run the Puppy ***
            call "SENDCMD" (uw$ & "UWVBRUN" & cdir$ & function$ & uw$)
            wait% = timeout%
            if wait% < 0% then exit_routine
            if wait% = 0% then wait% = 60%

            REM *** Hide Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,1" & uw$)

            REM *** Now wait for response ***
            call "GETCMD" (wait%,return%,temp$)

            REM *** Restore Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,0" & uw$)

            if return% >= 0% then exit_routine
               call "ASKGUI" (16%, "Application Failed to Respond",      ~
                  "Please check to make sure that " & function$ &        ~
                  " is in the sub-directory " & cdir$, wait%)
            call "SENDCMD" (uw$ &"UWERRApplication failed to Respond"&uw$)

        exit_routine
            if gui% = 1% then call "SENDCMD" (uw$ & "UWMSG " & uw$)
            end
