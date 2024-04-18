        REM *************************************************************~
            *                                                           *~
            *   SSS   PPPP    CCC    SSS   M   M   AAA    SSS   H   H   *~
            *  S      P   P  C   C  S      MM MM  A   A  S      H   H   *~
            *   SSS   PPPP   C       SSS   M M M  AAAAA   SSS   HHHHH   *~
            *      S  P      C   C      S  M   M  A   A      S  H   H   *~
            *   SSS   P       CCC    SSS   M   M  A   A   SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SPCSMASH - CRUNCHES ALL THE SPACES OUT OF AN 80-CHARACTER *~
            *            MAX LENGTH STRING AND RETURNS THE RESULT IN THE*~
            *            CALLING VARIABLE.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/06/80 ! ORIGINAL                                 ! BCW *~
            *************************************************************

            sub "SPCSMASH" (line$)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.16.01 04/04/86 Physical inventory and miscel  "
L10022: REM *************************************************************
            call "SPCESMSH" (line$,0%)
            end
