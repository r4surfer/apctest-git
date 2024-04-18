        REM *************************************************************~
            *                                                           *~
            *   PPPP   RRRR   N   N  TTTTT   SSS    CCC   RRRR   N   N  *~
            *   P   P  R   R  NN  N    T    S      C   C  R   R  NN  N  *~
            *   PPPP   RRRR   N N N    T     SSS   C      RRRR   N N N  *~
            *   P      R   R  N  NN    T        S  C   C  R   R  N  NN  *~
            *   P      R   R  N   N    T     SSS    CCC   R   R  N   N  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRNTSCRN - USES THE "SCREEN" USERSUB TO GET THE CONTENTS  *~
            *            OF THE SCREEN INTO A NICE PRINT FILE THAT WE   *~
            *            THROW OFF TO THE PRINTER ONLINE INSTEAD OF     *~
            *            SPOOLED.  THIS IS DONE BY SETTING THE SPOOLER  *~
            *            COMMAND TO "O", OVERRIDING THE DEFAULT "H"     *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+---------------------WHAT-----------------+-WHO-*~
            * 01/22/80 ! ORIGINAL                                 ! BCW *~
            *************************************************************~

            sub "ZPRNTSCR"

            dim a$22

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10027
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
L10027: REM *************************************************************
            a$ = " "
            call "EXTRACT" addr("CF", str(a$,2,8))
            str(a$,1,1) = "#"
            str(a$,6,16) = " "

            close ws
            u%=u%
            call"SCREEN"addr("C",u%,"P", a$)
              end
