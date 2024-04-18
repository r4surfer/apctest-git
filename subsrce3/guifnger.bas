        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGGG  U   U  IIIII  FFFFF  N   N   GGGG  EEEEE  RRRR    *~
            *  G      U   U    I    F      NN  N  G      E      R   R   *~
            *  G GGG  U   U    I    FFF    N N N  G GGG  EEE    RRRR    *~
            *  G   G  U   U    I    F      N  NN  G   G  E      R   R   *~
            *   GGG    UUU   IIIII  F      N   N   GGG   EEEEE  R   R   *~
            * FNGER = FiNd Gui Entity Response                          *~
            *-----------------------------------------------------------*~
            * GUIFNGER - This routine will finger the client process    *~
            *            waiting for data to be sent from the client to *~
            *            the host/server process.  Response from the    *~
            *            client should be signaled via a Function Key   *~
            *            value (from the CoStar VB enabled app use ^F01,*~
            *            etc) to generate a function key sequence).     *~
            *            Users may interrupt the waiting process by     *~
            *            pressing a function key!                       *~
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
            * 12/07/95 ! Original                                 ! LDJ *~
            * 03/24/97 ! Replaced WSXIO with GETCMD               ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GUIFNGER" (timeout%,     /* Timeout period in seconds.  If  ~
                                         0 assumes default timeout period~
                                         If timeout expires control is   ~
                                         returned to the caller - this is~
                                         NOT an error condition!     */  ~
                        len%,         /* IN:  not used                   ~
                                         OUT: Length of Data Received -  ~
                                              if any.                  */~
                        data$,        /* Receiver for any Data Sent IF   ~
                                         LEN% > 0.                     */~
                        return%)      /* < 0 = Timeout occurred          ~
                                          99 = Not GUI mode or error     ~
                                        >= 0 = Function key equivalent */

        dim                                                              ~
            uw$1,                        /* CoStar Magic Character     */~
            data$80                      /* PC Files directory         */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


            return% = 99%

        REM *** Are we in GUI Land ? ***
            call "CHECKGUI" addr(gui%)
            if gui% = 0% then exit_routine

            uw$ = hex(7f)
            call "SENDCMD" (uw$ & "UWMSGWaiting for Client Response, "   ~
                          & "Press F16 to Cancel/Exit" & uw$)
            if timeout% = 0% then wait% = 60% else wait% = timeout%

            REM *** Hide Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,1" & uw$)

            REM *** Now wait for response ***
            call "GETCMD" (wait%,return%,data$)
            len% = len(data$)

            REM *** Restore Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,0" & uw$)

            REM *** Clear Messsage area on Status Line ***
            call "SENDCMD" (uw$ & "UWMSG " & uw$)

        exit_routine
            end
