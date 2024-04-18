        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   H   H  EEEEE   CCC   K   K   GGG   U   U  IIIII   *~
            *  C      H   H  E      C      K  K   G      U   U    I     *~
            *  C      HHHHH  EEEE   C      KKK    G GGG  U   U    I     *~
            *  C      H   H  E      C      K  K   G   G  U   U    I     *~
            *   CCC   H   H  EEEEE   CCC   K   K   GGG    UUU   IIIII   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHECKGUI - General routine for testing whether or not     *~
            *            code is running with Co-Star Emulator.         *~
            *            Returns -1 if Not, 1 if yes.                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/22/94 ! Original  (stub only for testing)        ! LDJ *~
            * 03/21/96 ! No longer pauses on Close when programs  ! LDJ *~
            *          ! run outside of CMSCONTR in character mode!     *~
            * 04/16/97 ! Replaced WSXIO with GETCMD for NT.       ! LDJ *~
            * 04/21/97 ! Replaced lost code to set Env Variable   ! LDJ *~
            * 05/12/97 ! Add line to BACKSPACE out the UW string  ! LDJ *~
            * 12/01/97 ! Add Check Regardless code for zmanual.   ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        sub "CHECKGUI" addr(gui%)

        dim response$2,uw$1                      /* Query Response*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            If gui% <> -25% then standard_check
            if been_here_before% <> 31% then check_regardless
            goto exit_sub

check_regardless
            uw$ = hex(7f)
            ret% = 0%
            call "SENDCMD" (uw$ & "UW" & uw$)
            call "GETCMD" (2%, ret%, response$)
            if str(response$,,1%) <> "Y" then ret% = 0% else ret% = 1%
            if ret% = 0% then call "SENDCMD" (hex(0808)) /* remove UW */
            been_here_before% = 31%
            goto exit_sub
            
standard_check
            on been_here% goto exit_sub
            been_here% = 1% : guy% = -99%
            call "CMSTERM" addr(0%, guy%)
            if guy% >= 0% then exit_sub
            guy% = 0%
            uw$ = hex(7f)
            call "SENDCMD" (uw$ & "UW" & uw$)
            call "GETCMD" (2%, guy%, response$)
            if str(response$,,1%) <> "Y" then guy% = 0% else guy% = 1%
            if guy% = 0% then call "SENDCMD" (hex(0808)) /* remove UW */
            call "CMSTERM" addr(1%,guy%) /* Set Env Variable */

        exit_sub
            if gui% = -25% then gui% = ret% else gui% = guy%
            end
