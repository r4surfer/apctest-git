        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   AAA    SSS   K  K    GGG   U   U  IIIII                 *~
            *  A   A  S      K K    G      U   U    I                   *~
            *  AAAAA   SSS   KK     G GGG  U   U    I                   *~
            *  A   A      S  A K    G   G  U   U    I                   *~
            *  A   A   SSS   A  K    GGG    UUU   IIIII                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKGUI   - General routine for display of the WINDOWS     *~
            *            Message Box API (utilizing CoStar's mbox.scr   *~
            *            script to invoke same).  Note that the work-   *~
            *            station MUST be closed by the caller!          *~
            *            Calling arguments are;                         *~
            *                                                           *~
            * CALL "ASKGUI" (TYPE%, TITLE$, MESSAGE$, RETURN_VALUE%)    *~
            *                                                           *~
            *            See CoStar's Message Box macro for info on the *~
            *            arguments valid values - particularly TYPE% and*~
            *            RETURN_VALUE%. Do NOT though add the 128 to    *~
            *            TYPE% - that is done for you here.             *~
            *                                                           *~
            *            TYPE% = Add the values for the desired Button  *~
            *                    Type and Icon Type together to come up *~
            *                    with the TYPE% value.                  *~
            *               BUTTON TYPES;                               *~
            *                   Value   Description                     *~
            *                   -----   ------------------------        *~
            *                      0    OK                              *~
            *                      1    OK, Cancel                      *~
            *                      2    Abort, Retry, Ignore            *~
            *                      3    Yes,No,Cancel                   *~
            *                      4    Yes,No                          *~
            *                      5    Retry,Cancel                    *~
            *                                                           *~
            *               ICON TYPES;                                 *~
            *                     16    Stop Sign Icon                  *~
            *                     32    Question Mark Icon              *~
            *                     48    Exclamation Mark Icon           *~
            *                     64    Asterisk Icon                   *~
            *                                                           *~
            *            TITLE$ = text to place in the dialog box's     *~
            *                     title bar.                            *~
            *                                                           *~
            *            MESSAGE$ = Your text message to the user.      *~
            *                       (up to 160 characters in CHUI mode) *~
            *                                                           *~
            *            RETURN_VALUE% = Depending on the Button Type   *~
            *                            selected, one of the following *~
            *                            values will be returned.       *~
            *                                                           *~
            *                      1    OK selected                     *~
            *                      2    Cancel ""                       *~
            *                      3    Abort                           *~
            *                      4    Retry                           *~
            *                      5    Ignore                          *~
            *                      6    Yes                             *~
            *                      7    No                              *~
            *                     -1    Invalid response                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/13/95 ! Original                                 ! LDJ *~
            * 03/24/97 ! Replaced WSXIO with GETCMD. Expanded doc.! LDJ *~
            *          ! Added CHUI mode option.                  !     *~
            * 05/20/97 ! QC Fixes, better handling of CHUI mode.  ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            sub "ASKGUI" (type%, title$, message$, return_value%)

        dim message$237,                                                 ~
            message$(3)79,           /* Message Box Contents           */~
            msg$(3)79,               /* CHUI Mode Msg area             */~
            prompt$60,               /* CHUI Mode Buttons Prompt       */~
            return$3,                /* value back from GETCMD         */~
            title$80,                /* Message Box Title Bar Contents */~
            type$3,                  /* Message Box Type               */~
            uw$1                     /* CoStar Magic Character         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            return_value% = -1%
            message$() = message$
            call "CHECKGUI" addr(gui%)
            if gui% = 0% then ask_chui
            tran(title$,";,")replacing : tran(message$(),";,")replacing
            convert type% + 128% to type$, pic(##0)
            tran(message$(),hex(200d))replacing
            uw$ = hex(7f)
            call "SENDCMD" (uw$ & "UWSCRIPTsdk/mbox.scr" & uw$ & type$ & ~
                  "," & title$ & "," & str(message$(),,len(message$())) & uw$)
            /* Get Result */
WaitLoop:   call "GETCMD" (9999%,guy%,return$)
            if guy% < 0% then WaitLoop
            if guy% > 0% then end_routine   /* No ENTER Received */
            convert return$ to return_value%, data goto end_routine

        end_routine
            end

ask_chui:   /* Character Mode version */
            call "TXTSMASH" (message$(),msg$())
            button% = mod(type%,16%)
            on button%+1% gosub type0, type1,type2,type3,type4,type5
            return_value% = 2%
            tran(msg$(),hex(200d200a))replacing
            call "ASKUSER" (return_value%,title$,msg$(1%),msg$(2%),prompt$)
            return_value% = return_value% + 1%
            if return_value% < 1% or return_value% > 7% then return_value% = -1%
            goto end_routine

type0:      prompt$ = "(Press ENTER (OK) to Acknowledge)"
            return

type1:      prompt$ = "(Press ENTER for OK, F1 to Cancel)"
            return

type2:      prompt$ = "(Press F2 to Abort, F3 to Retry, F4 to Ignore)"
            return

type3:      prompt$ = "(Press F1 to Cancel, F5 for Yes, F6 for No)"
            return

type4:      prompt$ = "(Press F5 for Yes, F6 for No)"
            return

type5:      prompt$ = "(Press F1 to Cancel, F3 to Retry)"
            return
