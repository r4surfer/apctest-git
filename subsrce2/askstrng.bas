        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K    SSS   TTTTT  RRRR   N    N   GGG   *~
            *   A A   S      K K    S        T    R   R  N N  N  G      *~
            *  AAAAA   SSS   KK      SSS     T    RRRR   N  N N  G  GG  *~
            *  A   A      S  K K        S    T    R  R   N   NN  G   G  *~
            *  A   A   SSS   K  K    SSS     T    R   R  N    N   GGG   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKSTRNG - General routine for requesting a string        *~
            *            from User.  Overlays message window/box onto   *~
            *            current screen & waits for user response.      *~
            *            User can enter a string of characters.  If     *~
            *            a channel number is passed in, then the        *~
            *            string is validated against that file via      *~
            *            GETCODE (assumes primary key).                 *~
                         F1% passed back indicates validation success.  *~
            *            Caller controls whether message box appears at *~
            *            top or bottom of screen, title of box, and an  *~
            *            input message.                                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/23/93 ! Original                                 ! WPH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

           sub "ASKSTRNG"(arg1, arg2, arg3, arg4, arg5, arg6, arg7,      ~
                          arg8, arg9)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            close ws

            call "NARGS" addr(args%)
            if args% <> 9% then L02040     /*they didn't pass file stuff*/


            call "ZASKSTRN"(arg1, arg2, arg3, arg4, arg5, arg6, arg7,    ~
                          arg8, arg9)

            goto end_routine

L02040:     call "ZASKSTRN"(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

        end_routine
            end
