        REM *************************************************************~
            *                                                           *~
            *  PPPP   U   U  TTTTT  PPPP    AAA   RRRR   EEEEE  N   N   *~
            *  P   P  U   U    T    P   P  A   A  R   R  E      NN  N   *~
            *  PPPP   U   U    T    PPPP   AAAAA  RRRR   EEEE   N N N   *~
            *  P      U   U    T    P      A   A  R   R  E      N  NN   *~
            *  P       UUU     T    P      A   A  R   R  EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PUTPAREN - STUPID LITTLE SUBROUTINE THAT PUTS PARENTHESES *~
            *            AROUND A BLOCK OF TEXT.  EASIER TO DO IT HERE  *~
            *            THAT FUTSING AROUND IN MAIN PROGRAM FOR IT.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 04/18/80 ! ORIGINAL (PROBABLY NEVER TO BE MODIFIED) ! BCW *~
            *************************************************************

        sub "PUTPAREN" (text$)
            dim junktext$50
            junktext$ = " "
            if text$ = " " then end
            junktext$ = "(" & text$ & ")"
            text$ = junktext$
            end
